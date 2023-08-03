-- Lua serial communications library.
--
-- Note: currently only works on Win32.
--
-- The implementation uses alien ( http://alien.luaforge.net/ ).
--
-- Warning: not extensively tested.
--
-- Possible improvements:
--   - decide more consistently when to raise or return on error
--     (io.open/fh:read/fh:write return, but Lua
--      Programming Gems, p.137 suggests we perhaps
--      should raise)
--   - add close() method on file handle, possibly
--     via __gc on newproxy() so that it closes upon
--     garbage collection
--
-- D.Manura. 2008-07
-- S.Slobodov, 2009-01: fixed alien interface, fixed a typo,
--    added COM10 and up, made non-blocking
-- D.Manura, 2009-01: add close(); improve exception safety some
-- S.Slobodov, 2009-01: added routines to set DTR and RTS and to query CTS, plus unbuffered transmission
-- Licensed under the same terms as Lua itself (MIT license).
-- Please post patches and improvements.


local M = {}

local alien = require "alien"

-- win32 values for CreateFile
local GENERIC_READ = 0x80000000
local GENERIC_WRITE = 0x40000000
local OPEN_EXISTING = 3

-- win32 parity values
local EVENPARITY = 2
local MARKPARITY = 3
local NOPARITY = 0
local ODDPARITY = 1
local SPACEPARITY = 4

-- win32 stop bit values
local ONESTOPBIT = 0
local ONE5STOPBITS = 1
local TWOSTOPBITS = 2

-- maps parity name to win32 parity value
local parity_to_win32 = {
  even = EVENPARITY,
  mark = MARKPARITY,
  none = NOPARITY,
  odd = ODDPARITY,
  space = SPACEPARITY
}

-- maps stop bits to win32 stop bits value
local stopbits_to_win32 = {
  [1]   = ONESTOPBIT,
  [1.5] = ONE5STOPBITS,
  [2]   = TWOSTOPBITS
}

local kernel32 = alien.load"kernel32.dll"

-- bitwise operators
-- based on http://ricilake.blogspot.com/2007/10/iterating-bits-in-lua.html
-- 1-based indexing
local function bit(p) return 2 ^ (p - 1) end
local function hasbit(x, p) return x % (p + p) >= p end
local function setbit(x, p) return hasbit(x, p) and x or x + p end
local function clearbit(x, p) return hasbit(x, p) and x - p or x end
local function changebit(x, p, b) return b and setbit(x,p) or clearbit(x,p) end


local function get_last_error()
  local FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x00000100
  local FORMAT_MESSAGE_FROM_SYSTEM     = 0x00001000
  local FORMAT_MESSAGE_IGNORE_INSERTS  = 0x00000200

  local LANG_NEUTRAL    = 0x00
  local SUBLANG_DEFAULT = 0x01

  local function MAKELANGID(p, s) return s * 2^10 + p end

  -- C function declarations
  local FormatMessage = assert(kernel32.FormatMessageA)
  FormatMessage:types{ret ='int', abi = 'stdcall', 'int',
      'pointer', 'int', 'int', 'pointer', 'int', 'pointer'}
  local GetLastError = assert(kernel32.GetLastError)
  GetLastError:types{ret = 'int', abi = 'stdcall'}
  local LocalFree = assert(kernel32.LocalFree)
  LocalFree:types{ret = 'pointer', abi = 'stdcall', 'pointer'}

  local buf = alien.buffer(4)
  buf:set(0, buf2, 'pointer')

  local ret = FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER + FORMAT_MESSAGE_FROM_SYSTEM
      + FORMAT_MESSAGE_IGNORE_INSERTS,
    nil,
    GetLastError(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    buf, -- ref: msg_buf,
    0,
    nil
  )
  if ret == 0 then return "Unknown error." end
  local msg = alien.tostring(buf:get(1,'pointer'))
  LocalFree(buf:get(1, 'pointer'))

  return msg
end


local function config(h, t)
  local speed = t.speed
  local databits = t.databits
  local stopbits = t.stopbits
  local parity = t.parity
  local handshake = t.handshake

  assert(stopbits_to_win32[stopbits])
  assert(parity_to_win32[parity])
  assert(databits == 5 or databits == 6 or databits == 7 or databits == 8)
  assert(type(speed) == 'number' and speed > 0)

  -- C function declarations
  local GetCommState = kernel32.GetCommState
  GetCommState:types{ret = "int", abi = 'stdcall', "pointer", "pointer"}
  local SetCommState = assert(kernel32.SetCommState)
  SetCommState:types{ret = "int", abi = 'stdcall', "pointer", "pointer"}
  local SetCommTimeouts = assert(kernel32.SetCommTimeouts)
  SetCommTimeouts:types{ret = "int", abi = 'stdcall', "pointer", "pointer"}

  local buf = alien.buffer(4*3 + 3*2 + 3*1 + 5*3 + 2*1)
  GetCommState(h, buf)

  local offset_DCBlength = 1
  local offset_BaudRate  = 1 + 1*4
  local offset_flags     = 1 + 2*4
  local offset_XonLim    = 1 + 3*4 + 1*2
  local offset_XoffLim   = 1 + 3*4 + 2*2
  local offset_ByteSize  = 1 + 3*4 + 3*2
  local offset_Parity    = 1 + 3*4 + 3*2 + 1
  local offset_StopBits  = 1 + 3*4 + 3*2 + 2
  local offset_XonChar   = 1 + 3*4 + 3*2 + 3
  local offset_XoffChar  = 1 + 3*4 + 3*2 + 4
  local offset_ErrorChar = 1 + 3*4 + 3*2 + 5
  local offset_EofChar   = 1 + 3*4 + 3*2 + 6
  local offset_EvtChar   = 1 + 3*4 + 3*2 + 7

  buf:set(offset_BaudRate, speed, 'int')

  local bits = buf:get(offset_flags, 'int')
  bits = changebit(bits, bit(1),  true)  -- fBinary
  bits = changebit(bits, bit(2),  parity ~= 'none')  -- fParity
  bits = changebit(bits, bit(3),  handshake == 'hardware') -- fOutxCtsFlow
  bits = changebit(bits, bit(4),  false) -- fOutxDsrFlow (ok?)
  bits = changebit(bits, bit(5),  false) -- fDtrControl[1] (ok?)
  bits = changebit(bits, bit(6),  false) -- fDtrControl[2] (ok?)
  bits = changebit(bits, bit(7),  false) -- fDsrSensitivity (ok?)
  bits = changebit(bits, bit(8),  false) -- fTXContinueOnXoff  (ok?)
  bits = changebit(bits, bit(9),  handshake == 'xon/xoff') -- fOutX
  bits = changebit(bits, bit(10), handshake == 'xon/xoff') -- fInX
  bits = changebit(bits, bit(11), false) -- fErrorChar (ok?)
  bits = changebit(bits, bit(12), false) -- fNull (ok?)
  bits = changebit(bits, bit(13), false) -- fRtsControl [1]
  bits = changebit(bits, bit(14), handshake == 'xon/xoff') -- fRtsControl [2]
  bits = changebit(bits, bit(15), false) -- fAbortOnError (ok?)
  buf:set(offset_flags, bits, 'int')
  buf:set(offset_ByteSize, databits, 'byte')
  buf:set(offset_Parity, parity_to_win32[parity], 'byte')
  buf:set(offset_StopBits, stopbits_to_win32[stopbits], 'byte')

  SetCommState(h, buf)

  -- timeout on receive immediately if no data pending
  --	(so that you have a chance to do bigger an better things)
  -- http://msdn.microsoft.com/en-us/library/aa363437(VS.85).aspx
  local buf = alien.buffer(4*5)      -- _COMMTIMEOUTS:
  buf:set(1, -1, "int")              -- ReadIntervalTimeout
  buf:set(1+4*1, 0, "int")           -- ReadTotalTimeoutMultiplier
  buf:set(1+4*2, 0, "int")           -- ReadTotalTimeoutConstant
  buf:set(1+4*3, 0, "int")           -- WriteTotalTimeoutMultiplier
  buf:set(1+4*4, 0, "int")           -- WriteTotalTimeoutConstant
  SetCommTimeouts(h, buf)
end

local CloseHandle
local function open(t)
  local port = t.port
  assert(type(port) == 'string' and port:match('^COM[0-9]+$'),
    'invalid port name '..port..'; expecting e.g. "COM1"')

  local portNum = port:match("^COM([0-9]+)")
  if portNum+0 >= 10 then port = "\\\\.\\"..port end

  -- C function declarations
  local CreateFile = assert(kernel32.CreateFileA)
  CreateFile:types{ret = "pointer", abi = 'stdcall', "string",
      "int", "int", "pointer", "int", "int", "pointer"}
  CloseHandle = assert(kernel32.CloseHandle)
  CloseHandle:types{ret = 'int', abi = 'stdcall', "pointer"}

  local n = (GENERIC_READ + GENERIC_WRITE) - 2^32 + 1
  local h = CreateFile(port, n, 0, nil, OPEN_EXISTING, 0, nil)

  -- convert handle to int
  local tmp = alien.buffer(4)
  tmp:set(1, h, 'pointer')
  local hnum = tmp:get(1, 'int')
  if hnum == -1 then return nil, get_last_error() end

  local ok, msg = pcall(config, h, t)
  if not ok then
    CloseHandle(h)
    return nil, msg
  end

  return h
end
M.open = open

local function close(h)
  CloseHandle(h)
end
M.close = close

local function send(h, s)
  local WriteFile = assert(kernel32.WriteFile)
  WriteFile:types{ret="int", abi = 'stdcall', "pointer", "string",
      "int", "ref int", "pointer"}

  local bytes_written_buf = alien.buffer(4)

  local ret, nwritten = WriteFile(h, s, #s, 0, nil )
  if ret == 0 or nwritten ~= #s then
    error("failed write: " .. get_last_error())
  end
end
M.send = send

local function receive(h)
  -- C function declarations
  local ReadFile = assert(kernel32.ReadFile)
  ReadFile:types{ ret = 'int', abi = 'stdcall', "pointer",
      "ref char", "int", "ref int", "pointer"}

  local ret, char, nread = ReadFile(h, buffer_buf, 1, 0, nil )
  if ret == 0 then
    error("failed read: " .. get_last_error())
  end
  if char < 0 then char = char+256 end	-- avoid negatives
  return nread == 1 and string.char(char) or nil
end
M.receive = receive

local function receive_all(h)
  local s = ""
  while 1 do
    local c = receive(h)
    if c then
      s = s .. c
    else
      return s
    end
  end
end
M.receive_all = receive_all

local buf = ""
local function async_read_until(h, char)
  while 1 do
    local c = receive(h)
    if c then
      buf = buf .. c
      if c == char then
        local s = buf
        buf = ""
        return s
      end
    else
      return
    end
  end
end

local function SetDTR(h, set)
	local EscapeCommFunction = assert(kernel32.EscapeCommFunction)
	EscapeCommFunction:types{ret="int", abi = 'stdcall', "pointer", "int"}
	if set then
		EscapeCommFunction(h, SETDTR)
	else
		EscapeCommFunction(h, CLRDTR)
	end
end
M.SetDTR = SetDTR

local function SetRTS(h, set)
	local EscapeCommFunction = assert(kernel32.EscapeCommFunction)
	EscapeCommFunction:types{ret="int", abi = 'stdcall', "pointer", "int"}
	if set then
		EscapeCommFunction(h, SETRTS)
	else
		EscapeCommFunction(h, CLRRTS)
	end
end
M.SetRTS = SetRTS

local function GetCTS(h)
	local GetCommModemStatus = assert(kernel32.GetCommModemStatus)
	GetCommModemStatus:types{ret = "int", abi = "stdcall", "pointer", "ref int"}
	local _, CTS = GetCommModemStatus(h, 0)
	return CTS == CTS_ON
end
M.GetCTS = GetCTS

local function TransmitChar(h, c)
	local TransmitCommChar = assert(kernel32.TransmitCommChar)
	TransmitCommChar:types{ret="int", abi = 'stdcall', "pointer", "int"}
	if not TransmitCommChar(h, c) then return nil, get_last_error()
	else return true end
end
M.TransmitChar = TransmitChar

return M

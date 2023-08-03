local cocreate, resume, yield, costatus =
  coroutine.create, coroutine.resume, coroutine.yield, coroutine.status

local LOG = function(...)
  io.stderr:write(os.date"!%F %T ")
  io.stderr:write(...)
  io.stderr:write"\n"
end

-- This function expects to be a coroutine. When it is started,
-- it initializes itself, and then awaits further instructions.
-- Whenever it has nothing else to do it yields, unless it was
-- told to quit in which case it returns.

function supervisor(...)
  -- each event is a table of sets of coroutines;
  -- we assume we can handle the events in any order.
  local events = {}
  -- This table associates coroutines with names, mostly
  -- for debugging
  local name = {}
  -- to signal that we're done
  local done = false

  -- This function removes all references to a coro
  -- from all events, which we do when the coro dies.
  -- We could use weak tables to do the same thing.
  -- On the other hand, we might want to clean up other
  -- data associated with the coroutine. Anyway, this is easy:

  local function remove(coro)
    for _, coros in pairs(events) do
      coros[coro] = nil
    end
    name[coro] = nil
  end

  -- Convenience function to log tracebacks
  local function log_traceback(coro, msg, err)
    LOG("Coroutine ", name[coro], " ", msg, ":")
    local tb = debug.traceback(coro, err)
    for line in tb:gmatch"[^\n]+" do LOG("  ", line) end
  end

  -- The core routine, which handles the results of a
  -- coroutine.resume. First argument is the coro, rest
  -- are the return values from coro.resume(). The only
  -- thing we're expecting from the coro is a "waitfor"
  -- command, but other ones could be added...
  local function handle_resume(coro, ok, todo, ...)
    if costatus(coro) == "dead" then
      remove(coro) -- always get rid of it.
      -- log a traceback on error
      if not ok then log_traceback(coro, "failed with error", todo) end
    elseif todo ~= "waitfor" then
      -- todo should be "waitfor" and ... the event.
      -- If we had more verbs, we could use a case-table.
      log_traceback(coro, "unknown request "..tostring(todo),
                          "bad return value")
      remove(coro)
    else
      -- We don't care if the event doesn't exist (should we?)
      local q = events[...]
      if q == nil then q = {}; events[...] = q end
      -- We might want to tell the upper level that a new
      -- event needs to be recognized, though.
      -- if next(q) == nil then addevent((...)) end
      q[coro] = true
    end
  end 
      
  -- A table of actions; essentially a case statement
  local handler = {}

  -- do a clean exit (although actually there's no
  -- cleanup necessary, but there might be.)
  function handler.done()
    done = true
  end

  -- debugging: report on status
  function handler.status()
    -- invert the events table for nicer printing
    local n, e = {}, {}
    for evt, coros in pairs(events) do
      for coro in pairs(coros) do
        local who = name[coro]
        n[#n+1] = who
        e[who] = evt
      end
    end
    -- sort the names
    table.sort(n)
    -- and produce the report
    for _, who in ipairs(n) do
      LOG(who, " is waiting for ", tostring(e[who]))
    end
  end

  -- introduce a new actor (coroutine) into the system, and run
  -- it until it blocks
  function handler.introduce(who, func, ...)
    local coro = cocreate(func)
    name[coro] = who
    -- let it initialize itself
    return handle_resume(coro, resume(coro, ...))
  end

  -- send an event to whoever cares
  function handler.signal(what, ...)
    local q = events[what]
    if q and next(q) then
      for coro in pairs(q) do
        q[coro] = nil  -- handled
        handle_resume(coro, resume(coro, what, ...))
      end
      -- Maybe tell the top-level whether the event is
      -- still active?
      return next(q) ~= nil
    else
      -- No-one cares, sniff. "Log" the fact
      LOG("Event ", tostring(what), " dropped into the bit bucket")
    end
  end

  -- Set the __index meta for the handler table to avoid having
  -- to test for bad commands explicitly
  local function logargs(...)
    local t = {n = select("#", ...), ...}
    if t.n > 0 then
      for i = 1, t.n do
        local ti = t[i]
        t[i] = (type(ti) == "string" and "%q" or "%s")
               :format(tostring(ti))
      end
      LOG("..with arguments: ", table.concat(t, ", "))
    end
  end
  function handler:__index(what)
    LOG("Supervisor received unknown message ", what)
    return logargs
  end
  setmetatable(handler, handler)

  -- auxiliary function to handle a command, necessary to
  -- capture multiple returns from yield()
  local function handle(simonsays, ...)
    return handler[simonsays](...)
  end

  -- The main loop is a bit anti-climactic
  LOG"Starting up"
  local rv = handle(...)
  repeat 
    rv = handle(yield(rv))
  until done
  LOG"Shutting down"
end

------------------------------------------------------------------------------
--  Sample very simple processor
------------------------------------------------------------------------------

local function block(event) return yield("waitfor", event) end
local function process(n)
  for i = 1, n do
    local e, howmany = block"TICK"
    assert(e == "TICK" and howmany == i)
  end
end

------------------------------------------------------------------------------
-- Test driver in Lua
------------------------------------------------------------------------------

local super = cocreate(supervisor)

local function check(ok, rv)
  if not ok then
    LOG("supervisor crashed")
    local tb = debug.traceback(super, rv)
    for line in tb:gmatch"[^\n]+" do LOG("  ", line) end
    os.exit(1)
  end
  return rv
end

local function send(msg, ...) return check(resume(super, msg, ...)) end

local nprocess, nreps = tonumber(arg[1]) or 10, tonumber(arg[2] or 12)
for i = 1, tonumber(nprocess) do
  send("introduce", ("p%04i"):format(i), process, nreps)
end
local j = 1
while send("signal", "TICK", j) do
  j = j + 1
end
-- This should be empty
send"status"
send"done"
LOG("Endcount ", j)

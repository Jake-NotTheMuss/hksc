require "copas"

local function simple(host, port, handler)
    return copas.addserver(assert(socket.bind(host, port)),
        function(c)
            return handler(copas.wrap(c), c:getpeername())
        end)
end

local function example_handler(c, host, port)
    local peer = host .. ":" .. port
    print("example connection from", peer)
    c:send("Hello\r\n")
    print("data from", peer, (c:receive"*l"))
    print("example termination from", peer)
end

local function daytime_handler(c, host, port)
    print("daytime connection from", host, port)
    c:send(os.date() .. '\r\n')
end

local function echo_handler(c, host, port)
    print("echo connection from", host, port)
    repeat
        local line = c:receive"*l"
        if line then c:send(line .. '\r\n') end
    until not line
    print("echo termination from", host, port)
end

local function discard_handler(c, host, port)
    print("discard connection from", host, port)
    repeat until not c:receive(100)
    print("discard termination from", host, port)
end

-- Use 0 to listen on the standard (privileged) ports.
local offset = ... or 10000

simple("*", offset + 7, echo_handler)
simple("*", offset + 9, discard_handler)
simple("*", offset + 13, daytime_handler)
simple("*", offset + 57, example_handler)

return copas.loop()

local posts = {"/latest", "/categories", "/-5/welcome", "/-6/sample-discussion", "/-7/sample-idea", "/-8/sample-question", "/-11/lorem-ipsum"}

request = function()
  return wrk.format(nil, posts[math.random(#posts)])
end

-- State of the setup/done runner
local counter = 1

setup = function(thread)
  thread:set("id", counter)
  counter = counter + 1
end

-- State of the thread init/request/response runners
local logfile
local requests

local log_request_latency = function(started_at, finished_at)
  logfile:write(string.format('%d,%d\n', started_at, finished_at))
  requests = requests + 1
  if requests % 1000 == 0 then
    logfile:flush()
  end
end

init = function(args)
  logfile = io.open('latencies' .. id .. '.csv', 'w')
  requests = 0
end

response = function(status, headers, body, started_at, finished_at)
  log_request_latency(started_at, finished_at)
end

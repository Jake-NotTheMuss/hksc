do 
  local meta = {}
  function meta:__index(k)
    if k == "tail" then
      local rv = self.gen(self)
      self.tail, self.gen = rv, nil
      return rv
    end
  end
  function Seq(h, gen)
    return setmetatable({head = h, gen = gen}, meta)
  end
end

function SMap(func, seq)
  return Seq(func(seq.head),
             function() return SMap(func, seq.tail) end)
end

function SMerge(seq1, seq2)
  local head1, head2 = seq1.head, seq2.head
  local step
  if head1 < head2 then
    function step() return SMerge(seq1.tail, seq2) end
  elseif head2 < head1 then
    function step() return SMerge(seq1, seq2.tail) end
    head1 = head2
  else
    function step() return SMerge(seq1.tail, seq2.tail) end
  end
  return Seq(head1, step)
end

function Times(k)
  if k then
    return function(x) return x * k end
  else
    return function(x, y) return x * y end
  end
end

function Hamming()
  local init = 1
  if Bignum then init = Bignum(init) end
  local seq = Seq(init)
  local seq2, seq3, seq5 =
        SMap(Times(2), seq), SMap(Times(3), seq), SMap(Times(5), seq)  
  function seq.gen() return SMerge(seq2, SMerge(seq3, seq5)) end
  return seq
end

function SeqTail(seq, k)
  for i = 1, k do
    seq = seq.tail
  end
  return seq
end

if arg then
  local start, finish, inc = tonumber(arg[1]), tonumber(arg[2]), tonumber(arg[3])
  if not start then start, finish, inc = 1, 20, 1 end
  local h = SeqTail(Hamming(), start-1)
  print("hamming", start, h.head)
  if finish then
    while start + inc <= finish do
      start = start + inc
      h = SeqTail(h, inc)
      print("hamming", start, h.head)
    end
  end
end

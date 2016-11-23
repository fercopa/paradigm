RubyVM::InstructionSequence.compile_option = {
  tailcall_optimization: true,
  trace_instruction: false
}
 
def fact(n, acc)
  puts caller
  return acc if n <= 1
  puts "print stack"
  fact(n-1, n*acc)
end
 
puts fact(5,1)

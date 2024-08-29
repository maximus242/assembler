#!/usr/bin/env ruby

# Function to calculate SYSV hash
def sysv_hash(str)
  hash = 0
  str.each_byte do |byte|
    hash = (hash << 4) + byte
    g = hash & 0xf0000000
    if g != 0
      hash ^= (g >> 24)
      hash ^= g
    end
  end
  hash & 0xffffffff # Ensure the hash is a 32-bit integer
end

# Function to convert to little-endian format
def to_little_endian(hash)
  [hash].pack("V").unpack("H*").first
end

# Function to convert to big-endian format
def to_big_endian(hash)
  [hash].pack("N").unpack("H*").first
end

# Check if a string was provided
if ARGV.empty?
  puts "Usage: #{$0} <string> [endian (little|big)]"
  exit 1
end

input_string = ARGV[0]
endian = ARGV[1] || "little"

hash_value = sysv_hash(input_string)

output = case endian.downcase
         when "little"
           to_little_endian(hash_value)
         when "big"
           to_big_endian(hash_value)
         else
           puts "Unknown endian type. Use 'little' or 'big'."
           exit 1
         end

puts "SYSV hash (#{endian} endian): 0x#{output}"

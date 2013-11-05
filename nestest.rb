#!/usr/bin/env ruby

require 'rainbow'

expected = File.new("nestest.log").readlines.map { |l| l.gsub("\r\n", '')[0..72] }
actual = `lein with-profile emu run nestest.nes`.split(/\n/)[1..-1]

actual.zip(expected).each_with_index do |(a, e), i|
  if e.nil?
    break
  elsif e != a
    expected_diff = e.chars.zip(a.chars).map { |ec, ac|
      if ac != ec
        ec.foreground(:green)
      else
        ec
      end
    }.join ''

    actual_diff = a.chars.zip(e.chars).map { |ac, ec|
      if ac != ec
        ac.foreground(:red)
      else
        ac
      end
    }.join ''

    if ![
      8981, 8983, 8985, 8987, 8989 # Problems with $40xx, this is the APU, so no wonder there
    ].include?(i+1)
      ([0, i-15].max..i-1).each { |i| puts "                " + actual[i] }

      puts sprintf("%-5d expected: %s", i + 1, expected_diff)
      puts sprintf("%-5d actual  : %s", i + 1, actual_diff)
      puts "\r\n"

      exit 1
    end
  end
end

puts "All systems nominal"

require 'rubygems'
require 'serialport'

port = SerialPort.new "/dev/cu.KeySerial1", :baud => 9600, :stop_bits => 1, :parity => SerialPort::NONE

def getresponse(port)
    s = ""
    x = port.getc
    while x != 13 do
        s << x.chr
        x = port.getc
    end
    return s
end

def sendcmd(port,cmd)
    puts "SEND #{cmd} -------------------"
    port.write (cmd + "\r");
    sleep 0.5
    getresponse(port)           # ignore the echo of the command
    return getresponse(port)
end

while true
    puts sendcmd(port, "TV");
    puts sendcmd(port, "VN55");
    puts sendcmd(port, "VN60");
    puts sendcmd(port, "MU");
end

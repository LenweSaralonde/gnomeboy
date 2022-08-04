local self = GnomeBoyAdvance
local gem = GBAgem
local mt = gem.GBZ80

mt.MRead = {}
local MRead = mt.MRead

local band = bit.band;
local bor = bit.bor;
local rshift = bit.rshift;
local lshift = bit.lshift;

mt.MWrite = {}
local MWrite = mt.MWrite


------------------------
-- READ Memory Ranges --
------------------------

local function ReadBiosSpace( self, Addr )
	if self.EnableBios then
		return self.BIOS[Addr]
	else
		return self.ROM[Addr]
	end
end

local function ReadRomZero( self, Addr )
	return self.ROM[Addr]
end

local function ReadRomOne( self, Addr )
	if self.CartMBCMode == 0 then
		return self.ROM[Addr]
	elseif self.CartMBCMode == 3 then
		return self.ROM[Addr + 0x4000*(self.RomBank-1)]
	end
end

local function ReadVideoRam( self, Addr )
	return self.Memory[Addr]
end

local function ReadExternalRam( self, Addr )
	if self.CartMBCMode == 3 then
		return self.RAM[Addr + 0x2000*self.RamBank ]
	end
end

local function ReadMainRam( self, Addr )
	return self.Memory[Addr]
end

local function ReadEchoRam( self, Addr )
	return self.Memory[Addr - 0x2000]
end

local function ReadSpriteRam( self, Addr )
	return self.Memory[Addr]
end

local function ReadHighRam( self, Addr )
	return self.Memory[Addr]
end


-------------------------
-- WRITE Memory Ranges --
-------------------------

local function RomBankNumber( self, Addr, Data )
	if Data == 0 then Data = 1 end

	if self.CartMBCMode == 3 then
		self.RomBank = band(Data,127)
	end
end

local function RamBankNumber( self, Addr, Data )

	if self.CartMBCMode == 3 and Data < 4 then
		self.RamBank = band(Data,3)
	end
end



local function WriteVideoRam( self, Addr, Data )
	self.Memory[Addr] = Data --Once LCD modes are added this can't be accessed during mode 3
end

local function WriteExternalRam( self, Addr, Data )
	if self.CartMBCMode == 3 then
		self.RAM[Addr + 0x2000*self.RamBank ] = Data
	end
end

local function WriteMainRam( self, Addr, Data )
	self.Memory[Addr] = Data
end

local function WriteEchoRam( self, Addr, Data )
	--Add support for Echo ram later, not sure how it works 100%, especially when it comes to writing.
end

local function WriteSpriteRam( self, Addr, Data )
	self.Memory[Addr] = Data -- Once LCD modes are added this can't be accessed during mode 2
end

local function WriteHighRam( self, Addr, Data )
	self.Memory[Addr] = Data
end

------------------------
-- Hardware registers --
------------------------

-- Timers
MRead[ 0xFF04 ] = function( self, Addr ) return self.Divider end -- Divider
MRead[ 0xFF05 ] = function( self, Addr ) return self.Timer end -- Timer
MRead[ 0xFF06 ] = function( self, Addr ) return self.TimerBase end -- What the timer resets to
MRead[ 0xFF07 ] = function( self, Addr ) return self.Memory[ 0xFF07 ] end -- Timer control register, only return first 3 bits?

MWrite[ 0xFF04 ] = function( self, Addr, Data ) self.Divider = 0 end -- Divider reset to 0 when written to
MWrite[ 0xFF05 ] = function( self, Addr, Data ) self.Timer = Data end -- Set timer
MWrite[ 0xFF06 ] = function( self, Addr, Data ) self.TimerBase = Data end -- Set timer base
MWrite[ 0xFF07 ] = function( self, Addr, Data )
	self.Memory[ 0xFF07 ] = band(Data,5) -- Better Safe than sorry
	self.TimerCounter = self.TimerDB[band(Data,0x3)] -- Set the timer incriment rate to the first 2 bits with a lookup DB
	self.TimerEnabled = band(Data,0x4) == 0x4 -- 3rd byte enables/disables the timer
end


-----------------
-- LCD and GPU --
-----------------

-- Background Scroll Y
MRead[ 0xFF42 ] = function( self, Addr ) return self.ScrollY end
MWrite[ 0xFF42 ] = function( self, Addr, Data ) self.ScrollY = Data end

-- Background Scroll X
MRead[ 0xFF43 ] = function( self, Addr ) return self.ScrollX end
MWrite[ 0xFF43 ] = function( self, Addr, Data ) self.ScrollX = Data end

-- Window Scroll X
MRead[ 0xFF4A ] = function( self, Addr ) return self.WindowY end
MWrite[ 0xFF4A ] = function( self, Addr, Data ) self.WindowY = Data end

-- Window Scroll Y
MRead[ 0xFF4B ] = function( self, Addr ) return self.WindowX end
MWrite[ 0xFF4B ] = function( self, Addr, Data ) self.WindowX = Data end



-- Current Scanline Register
MRead[ 0xFF44 ] = function( self, Addr ) return self.ScanlineY end
MWrite[ 0xFF44 ] = function( self, Addr, Data ) self.ScanLineY = 0 end -- Do nothing, can't write to scanline register

-- LY Compare
MRead[ 0xFF45 ] = function( self, Addr ) return self.CompareY end
MWrite[ 0xFF45 ] = function( self, Addr, Data ) self.CompareY = Data end

-- LCD Control Register
MRead[ 0xFF40 ] = function( self, Addr ) return self.Memory[Addr] end
MWrite[ 0xFF40 ] = function( self, Addr, Data )

	self.Memory[Addr] = Data

	self.LCDEnable = band(128,Data) == 128
	self.WindowMap = ((band(64,Data) == 64 and 0x9C00 or 0x9800))
	self.WindowEnable = band(32,Data) == 32
	self.TileData = (band(16,Data) == 16 and 0x8000 or 0x8800)
	self.BGMap =(band(8,Data) == 8 and 0x9C00 or 0x9800)
	self.SpriteSize = (band(4,Data) == 4 and 16 or 8)
	self.SpriteEnable = band(2,Data) == 2
	self.BGEnable = band(1,Data) == 1

end

-- LCD Status Regiter

MRead[ 0xFF41 ] = function( self, Addr )
	local Mem1 = 0
	Mem1 = bor(Mem1,(self.CoincidenceInterupt and 64 or 0))
	Mem1 = bor(Mem1,(self.ModeTwoInterupt and 32 or 0))
	Mem1 = bor(Mem1,(self.ModeOneInterupt and 16 or 0))
	Mem1 = bor(Mem1,(self.ModeZeroInterupt and 8 or 0))
	Mem1 = bor(Mem1,(self.CompareY == self.ScanlineY and 4 or 0))
	Mem1 = bor(Mem1,band(self.Mode,3))

	return Mem1
end

MWrite[ 0xFF41 ] = function( self, Addr, Data )
	self.CoincidenceInterupt = band(64,Data) == 64
	self.ModeTwoInterupt = band(32,Data) == 32
	self.ModeOneInterupt = band(16,Data) == 16
	self.ModeZeroInterupt = band(8,Data) == 8
end


--- JOYP

MRead[ 0xFF00 ]= function( self, Addr )
	if self.SelectDirectionKeys then
		return self.ButtonByte + (self.SelectDirectionKeys and 16 or 0) + (self.SelectButtonKeys and 32 or 0)
	elseif self.SelectButtonKeys then
		return self.DPadByte + (self.SelectDirectionKeys and 16 or 0) + (self.SelectButtonKeys and 32 or 0)
	end
end

MWrite[ 0xFF00 ] = function( self, Addr, Data )
	self.SelectDirectionKeys = band(Data,16) == 16
	self.SelectButtonKeys = band(Data,32) == 32
end


--- DMA Transfer

MRead[ 0xFF46 ]= function( self, Addr ) return 0 end

MWrite[ 0xFF46 ] = function( self, Addr, Data )
	DMAddr = lshift(Data,8)

	for n = 0, 0xA0 do
		self.Memory[ bor(0xFE00,n) ] = self.Memory[ bor(DMAddr,n) ]
	end
end







---------------
-- Interupts --
---------------

-- Interupt Enable
MRead[ 0xFFFF ] = function( self, Addr ) return self.IE end
MWrite[ 0xFFFF ] = function( self, Addr, Data ) self.IE = band(Data,0x1F) end

-- Interupt Request
MRead[ 0xFF0F ] = function( self, Addr )  return self.IF end
MWrite[ 0xFF0F ] = function( self, Addr, Data ) self.IF = band(0x1F,Data) end



-- Disable Bootrom
MRead[ 0xFF50 ] = function( self, Addr ) return 0 end
MWrite[ 0xFF50 ] = function( self, Addr ) self.EnableBios = false end






------------------------------
-- Define the memory ranges --
------------------------------

-- Read
for n = 0x0000, 0x00FF 	do MRead[n] = ReadBiosSpace end
for n = 0x0100, 0x3FFF 	do MRead[n] = ReadRomZero end
for n = 0x4000, 0x7FFF 	do MRead[n] = ReadRomOne end
for n = 0x8000, 0x9FFF 	do MRead[n] = ReadVideoRam end
for n = 0xA000, 0xBFFF 	do MRead[n] = ReadExternalRam end
for n = 0xC000, 0xDFFF 	do MRead[n] = ReadMainRam end
for n = 0xE000, 0xFDFF  do MRead[n] = ReadEchoRam end
for n = 0xFE00, 0xFE9F 	do MRead[n] = ReadSpriteRam end
for n = 0xFF80, 0xFFFE 	do MRead[n] = ReadHighRam end

-- Write

for n = 0x2000, 0x3FFF 	do MWrite[n] = RomBankNumber end
for n = 0x4000, 0x5FFF 	do MWrite[n] = RamBankNumber end

for n = 0x8000, 0x9FFF 	do MWrite[n] = WriteVideoRam end
for n = 0xA000, 0xBFFF 	do MWrite[n] = WriteExternalRam end
for n = 0xC000, 0xDFFF 	do MWrite[n] = WriteMainRam end
--Write Echo Ram
for n = 0xFE00, 0xFE9F 	do MWrite[n] = WriteSpriteRam end
for n = 0xFF80, 0xFFFE 	do MWrite[n] = WriteHighRam end


-- SOUND
-- https://gbdev.gg8.se/wiki/articles/Sound_Controller

-- Sweep (Channel 1)
MWrite[ 0xFF13 ] = function(self, Addr, Data)
	self.SoundFrequency[1] = band(self.SoundFrequency[1], 0x700) + Data
end
MWrite[ 0xFF14 ] = function(self, Addr, Data)
	self.SoundFrequency[1] = band(self.SoundFrequency[1], 0xFF) + band(lshift(Data, 8), 0x700)
	local frequency = 131072 / (2048 - self.SoundFrequency[1])
	self:SetChannelFrequency(1, frequency)
end
MWrite[ 0xFF12 ] = function(self, Addr, Data)
	local volume = rshift(band(Data, 0xF0), 4) / 0xF
	local envelope = band(Data, 0x07) * 1 / 64
	self:SetChannelVolume(1, volume, envelope)
end

-- Tone (Channel 2)
MWrite[ 0xFF18 ] = function(self, Addr, Data)
	self.SoundFrequency[2] = band(self.SoundFrequency[2], 0x700) + Data
end
MWrite[ 0xFF19 ] = function(self, Addr, Data)
	self.SoundFrequency[2] = band(self.SoundFrequency[2], 0xFF) + band(lshift(Data, 8), 0x700)
	local frequency = 131072 / (2048 - self.SoundFrequency[2])
	self:SetChannelFrequency(2, frequency)
end
MWrite[ 0xFF17 ] = function(self, Addr, Data)
	local volume = rshift(band(Data, 0xF0), 4) / 0xF
	local envelope = band(Data, 0x07) * 1 / 64
	self:SetChannelVolume(2, volume, envelope)
end

-- Wave (Channel 3)
MWrite[ 0xFF1D ] = function(self, Addr, Data)
	self.SoundFrequency[3] = band(self.SoundFrequency[3], 0x700) + Data
end
MWrite[ 0xFF1E ] = function(self, Addr, Data)
	self.SoundFrequency[3] = band(self.SoundFrequency[3], 0xFF) + band(lshift(Data, 8), 0x700)
	local frequency = 65536 / (2048 - self.SoundFrequency[3])
	self:SetChannelFrequency(3, frequency)
end
MWrite[ 0xFF1C ] = function(self, Addr, Data)
	local volumeLevel = rshift(band(Data, 0x60), 5)
	if volumeLevel == 2 then
		self:SetChannelVolume(3, .5, 1)
	elseif volumeLevel == 3 then
		self:SetChannelVolume(3, .25, 1)
	else
		self:SetChannelVolume(3, volumeLevel, 1)
	end
end

-- Noise (Channel 4)
MWrite[ 0xFF21 ] = function(self, Addr, Data)
	local volume = rshift(band(Data, 0xF0), 4) / 0xF
	self:SetChannelVolume(4, volume, 1)
end
MWrite[ 0xFF22 ] = function(self, Addr, Data)
	local s = rshift(band(Data, 0xF0), 4)
	local r = band(Data, 0x07) or .5
	local frequency = 524288 / r / math.pow(2, s + 1)
	self:SetChannelFrequency(4, frequency, 1)
end

------------------------
-- Main R/W Functions --
------------------------

function mt:Read(Addr)
	-- This checking shouldn't be necessary in the long run once all fringe cases are accounted for
	local func = self.MRead[Addr]
	if func then
	    return func( self, Addr )
	else
	    return self.Memory[Addr]
	end
end

function mt:Write(Addr,Data)
	-- This checking shouldn't be necessary in the long run once all fringe cases are accounted for
	local func = self.MWrite[Addr]
	if func then
		func( self, Addr, Data )
	else
		self.Memory[Addr] = Data
	end

end
















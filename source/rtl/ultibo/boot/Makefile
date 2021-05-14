PREFIX ?= arm-none-eabi-

TARGETPI2 = armstub32-rpi2
TARGETPI3 = armstub32-rpi3
TARGETPI4 = armstub32-rpi4

all: $(TARGETPI2).bin  $(TARGETPI3).bin  $(TARGETPI4).bin

$(TARGETPI2).bin: armstub7.S
	$(PREFIX)gcc -march=armv7-a -o $(TARGETPI2).o -c armstub7.S
	$(PREFIX)ld --section-start=.init=0 -o $(TARGETPI2).elf $(TARGETPI2).o
	$(PREFIX)objdump -D $(TARGETPI2).elf > $(TARGETPI2).lst
	$(PREFIX)objcopy $(TARGETPI2).elf -O binary $(TARGETPI2).bin

$(TARGETPI3).bin: armstub7.S
	$(PREFIX)gcc -march=armv7-a -DBCM2710=1 -o $(TARGETPI3).o -c armstub7.S
	$(PREFIX)ld --section-start=.init=0 -o $(TARGETPI3).elf $(TARGETPI3).o
	$(PREFIX)objdump -D $(TARGETPI3).elf > $(TARGETPI3).lst
	$(PREFIX)objcopy $(TARGETPI3).elf -O binary $(TARGETPI3).bin

$(TARGETPI4).bin: armstub7.S
	$(PREFIX)gcc -march=armv7-a -DGIC=1 -DBCM2710=1 -DBCM2711=1 -o $(TARGETPI4).o -c armstub7.S
	$(PREFIX)ld --section-start=.init=0 -o $(TARGETPI4).elf $(TARGETPI4).o
	$(PREFIX)objdump -D $(TARGETPI4).elf > $(TARGETPI4).lst
	$(PREFIX)objcopy $(TARGETPI4).elf -O binary $(TARGETPI4).bin

clean:
	rm -f *.o *.elf *.bin *.lst

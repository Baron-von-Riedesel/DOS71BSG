
 1. About
 
 DOS71BSG.BIN is a bootsector for MS-DOS 7.1 FAT32 partitions on a
 GPT-partitioned disk. Actually, it's 3 sectors, size 1536.
 
 
 2. Usage
 
 a) create a FAT32 partition on the disk - must be located entirely in the
    first 2 TB of the disk!
 b) use a disk editor (WDe) to save the bootsector to a file.
 c) copy the BPB of the saved sector ( bytes 0x0B-0x59 ) into DOS71BSG.BIN, at
    ther very same location.
 d) if unsure, use WDe to check if the modified DOS71BSG.BIN looks correct.
 e) write the modified DOS71BSG.BIN to the partitions boot sector ( 3 sectors ).
 f) ensure that MS-DOS 7.1 IO.SYS is in the partition's root directory.
 g) use grub-mkconfig to add the partition to grub's boot menu.
 
 No risk, no fun.

 
 
 

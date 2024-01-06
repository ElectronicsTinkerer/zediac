
print("tempos:")
print(f"    .word $ffff ; <ZERO> ")
for i in range(1,256):
    v = int(7370000*60/(256*i))
    if v > 65535:
        v = 65535
    elif v < 100:
        v = 100
    print(f"    .word {v:08d} ; {i: 3d} -> {7370000*60/(256*i)}")

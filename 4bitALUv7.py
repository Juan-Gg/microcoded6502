#ALU code for version 20 and up of logisim simualtion

#LOGICAL FUNCTIONS--------------------
def NOP(a,b):
    return a

def NOT(a,b):
    return ~a & 0b1111

def OR(a,b):
    return (a|b) & 0b1111

def XOR(a,b):
    return (a^b) & 0b1111

def AND(a,b):
    return (a&b) & 0b1111

def ROL(a,b):
    return (a<<1) & 0b11111

def ROLC(a,b):
    return ((a<<1)+1) & 0b11111

def ROR(a,b):
    out = (a>>1)
    if(a & 0b1):
        out = out | 0b10000 #Puts shifted out bit into carry
    return out | 0b10000000 #This signal enables a selector that switches carry bits around for shifting right.

def RORC(a,b):
    out = (a>>1) | 0b1000 #Shifts the carry bit into the MSB
    if(a & 0b1):
        out = out | 0b10000
    return out | 0b10000000


#ARITHMETIC FUNCTIONS-------------------
def SUM0(a,b):
    return (a) & 0b11111

def SUM0C(a,b):
    return (a+1) & 0b11111

def SUB0(a,b):
    return ((a-1) & 0b11111)^0b10000

def SUB0C(a,b):
    return ((a) & 0b11111)^0b10000

def ADD(a,b):
    return (a+b) & 0b11111

def ADDC(a,b):
    return (a+b+1) & 0b11111

def SUB(a,b): #Now carry is "borrow". It must be set by the programmer before subtracting
    return ((a-b-1) & 0b11111)^0b10000

def SUBC(a,b):
    return ((a-b) & 0b11111)^0b10000

#INC AND DEC DO NOT MAKE SENSE IF WE WANT THE SAME CODE FOR BOTH ROMS


#SIGN EXTEND. UNFORTUNATELY WE NEED TWO DIFFERENT ROM IMAGES BECAUSE OF THIS:
def LSE(a,b):
    return 0b0 | 0b10000000

def LSEC(a,b):
    return 0b1111 | 0b10000000

def HSE(a,b):
    if(a&0b1000):
        return 0b11111 | 0b10000000
    else:
        return 0b0 | 0b10000000

def BIT(a,b): #V, N, 6, 7 from a; Z from a&b
    out = 0
    if(a&b == 0):
        out = out | 0b00100000 # Z
    if(a&0b0100):
        out = out | 0b01000000 # V V flag is taken from MS 4-bit ALU
    if(a&0b1000):
        out = out | 0b00001000 # N Neg flag comes from MSB of result. Result is discarded, so we can do this.
    return out

#ALU LOW
#ALU HIGH
             # 0 NOP, 1 NOT, 2 OR, 3 XOR, 4 AND, 5 ROL, 6 ROR, 7 SUM0, 8 SUB0, 9 SUM, 10 SUB, 11 SE, 12 BIT ,13 NOP, 14 NOP, 15 NOP
#memoryMap = [NOP, NOP, NOT, NOT, OR, OR, XOR, XOR, AND, AND, ROL, ROLC, ROR, RORC, SUM0, SUM0C, SUB0, SUB0C, ADD, ADDC, SUB, SUBC,LSE,LSEC,BIT,BIT,NOP,NOP,NOP,NOP,NOP,NOP]
memoryMap = [NOP, NOP, NOT, NOT, OR, OR, XOR, XOR, AND, AND, ROL, ROLC, ROR, RORC, SUM0, SUM0C, SUB0, SUB0C, ADD, ADDC, SUB, SUBC,HSE,HSE,BIT,BIT,NOP,NOP,NOP,NOP,NOP,NOP]

print("v2.0 raw\n")
for i in range(len(memoryMap)):
    print("#"+str(i)+"---------------")
    for b in range(16):
        for a in range(16):
            out = memoryMap[i](a,b)
            #Flags:
            #Zero
            if(i!=24 and i!=25): #All but bit test (BIT)
                if out&0b1111 == 0:
                    out = out | 0b00100000
            #Overflow
            if(i==18 or i==19): #SUM operations:
                if ((a & 0b1000 and b & 0b1000 and not out & 0b1000) or \
                (not a & 0b1000 and not b & 0b1000 and  out & 0b1000)): 
                    out = out | 0b01000000
            
            if(i==20 or i==21): #SUB operations
                if ((a & 0b1000 and not b & 0b1000 and not out & 0b1000) or \
                    (not a & 0b1000 and b & 0b1000 and  out & 0b1000)):
                    out = out | 0b01000000

            #Format
            print(str(hex(out))[2:].zfill(2), end=' ')
        print()



#LOGISIM VERSION 35 ONWARDS.



# MISC
# Flags order: NV---IZC   Z  and I flags can be cleared and set, V can only be cleared.
#              76543210
#
#Address high reg can be written from Data Bus ['adhe', 'adhw'] or from Aux Bus ['adhe']

############## CONTROL LINES ##################
ctrlLines = ['rar','raw','alu0','alu1','alu2','alu3','rxr','rxw','ryr','ryw','spr','spw','pchr','pchw','pclr','pclw',
            'pcce','abw','adhr','adhw','adlr','adlw','irw','tcclr','extr','extw','pchra','adhe','sc','sz','si','sv',
            'sn','fgs','fgv','cg0', 'cg1', 'cg2', 'aaw', 'alr','srw','srr','hce','nmi','c','d','e','f']

activeLow = ['pchw','pclw','irw','spw','rxw','ryw','raw','aaw','abw','alr','pchr','pclr','pchra','spr','rxr','ryr','rar',
            'adhw','adhr','adlr','srr','adhe','adlw']

bits = {}

for i in range(48):
    bits[ctrlLines[i]] = (0b1<<i)

############## OPCODES #######################
definedOpcodes = {
#MISC
0xff:'rst', 0xea:'nop', 0xfc:'nmi',
#REGISTER TRANSFER
0xaa:'tax', 0x8a:'txa', 0x9a:'txs', 0xba:'tsx', 0x98:'tya', 0xa8:'tay', 
#ALU
0xe8:'inx', 0xc8:'iny', 0xe6:'inc_zp', 0xf6:'inc_zp_x', 0xee:'inc_ab', 0xfe:'inc_ab_x',
0x88:'dey', 0xca:'dex', 0xc6:'dec_zp', 0xd6:'dec_zp_x', 0xce:'dec_ab', 0xde:'dec_ab_x',
0x09:'ora_in', 0x05:'ora_zp', 0x15:'ora_zp_x', 0x0d:'ora_ab', 0x1d:'ora_ab_x', 0x19:'ora_ab_y', 0x01:'ora_x_id', 0x11:'ora_id_y',
0x29:'and_in', 0x25:'and_zp', 0x35:'and_zp_x', 0x2d:'and_ab', 0x3d:'and_ab_x', 0x39:'and_ab_y', 0x21:'and_x_id', 0x31:'and_id_y',
0x49:'eor_in', 0x45:'eor_zp', 0x55:'eor_zp_x', 0x4d:'eor_ab', 0x5d:'eor_ab_x', 0x59:'eor_ab_y', 0x41:'eor_x_id', 0x51:'eor_id_y',
0x69:'adc_in', 0x65:'adc_zp', 0x75:'adc_zp_x', 0x6d:'adc_ab', 0x7d:'adc_ab_x', 0x79:'adc_ab_y', 0x61:'adc_x_id', 0x71:'adc_id_y',
0xe9:'sbc_in', 0xe5:'sbc_zp', 0xf5:'sbc_zp_x', 0xed:'sbc_ab', 0xfd:'sbc_ab_x', 0xf9:'sbc_ab_y', 0xe1:'sbc_x_id', 0xf1:'sbc_id_y',
0x0a:'asl_a', 0x06:'asl_zp', 0x16:'asl_zp_x', 0x0e:'asl_ab', 0x1e:'asl_ab_x', 
0x4a:'lsr_a', 0x46:'lsr_zp', 0x56:'lsr_zp_x', 0x4e:'lsr_ab', 0x5e:'lsr_ab_x', 
0x2a:'rol_a', 0x26:'rol_zp', 0x36:'rol_zp_x', 0x2e:'rol_ab', 0x3e:'rol_ab_x', 
0x6a:'ror_a', 0x66:'ror_zp', 0x76:'ror_zp_x', 0x6e:'ror_ab', 0x7e:'ror_ab_x',
0xc9:'cmp_in', 0xc5:'cmp_zp', 0xd5:'cmp_zp_x', 0xcd:'cmp_ab', 0xdd:'cmp_ab_x', 0xd9:'cmp_ab_y', 0xc1:'cmp_x_id', 0xd1:'cmp_id_y',
0xe0:'cpx_in', 0xe4:'cpx_zp', 0xec:'cpx_ab', 
0xc0:'cpy_in', 0xc4:'cpy_zp', 0xcc:'cpy_ab', 
0x24:'bit_zp', 0x2c:'bit_ab',
#SET/CLEAR FLAGS
0x18:'clc', 0x38:'sec', 0x58:'cli', 0x78:'sei', 0xb8:'clv',
#LOAD/STORE 
0xa9:'lda_in', 0xa5:'lda_zp', 0xb5:'lda_zp_x', 0xad:'lda_ab', 0xbd:'lda_ab_x', 0xb9:'lda_ab_y', 0xa1:'lda_x_id', 0xb1:'lda_id_y',
0xa2:'ldx_in', 0xa6:'ldx_zp', 0xb6:'ldx_zp_y', 0xae:'ldx_ab', 0xbe:'ldx_ab_y', 
0xa0:'ldy_in', 0xa4:'ldy_zp', 0xb4:'ldy_zp_x', 0xac:'ldy_ab', 0xbc:'ldy_ab_x',
0x85:'sta_zp', 0x95:'sta_zp_x', 0x8d:'sta_ab', 0x9d:'sta_ab_x', 0x99:'sta_ab_y', 0x81:'sta_x_id', 0x91:'sta_id_y',
0x86:'stx_zp', 0x96:'stx_zp_y', 0x8e:'stx_ab',
0x84:'sty_zp', 0x94:'sty_zp_x', 0x8c:'sty_ab',
#STACK
0x48:'pha', 0x08:'php', 0x68:'pla', 0x28:'plp',
#JUMP/INTERRUPTS
0x00:'brk', 0x40:'rti',
0x20:'jsr', 0x60:'rts',
0x4c:'jmp_ab', 0x6c:'jmp_id',
#CONDITIONAL BRANCHES
0x90:'bcc', 0xb0:'bcs', 0xf0:'beq', 0x30:'bmi', 0xd0:'bne', 0x10:'bpl', 0x50:'bvc', 0x70:'bvs',
}
'''
'''
############ FETCH SEQUENCE ##################
fetchSequence =  [['pclr','adlw','pchra','adhe'], ['extr','irw','pcce']] 

############# BRANCH SEQUENCE ################
#Required flag has to be selected and t1 is not included here. 
#                       Select n, P.C. -> Addr regs;
#Example of a branch: [['sn','pclr', 'adlw','pchra','adhe']] + branch,
#                     [['sn','pclr', 'adlw','pchra','adhe']] + not_branch,

#              mem-> ALU A, PC count;      PCL-> ALU B, clear c;            (+)ALU -> PCL, save c;                              (SE)ALU->ALU A;                    PCH -> ALU B;        (+)ALU -> PCH
branch =     [['hce','extr','aaw','pcce'],['hce','pclr','abw','sc','fgs'],['hce','alu0','alu3','alr','pclw','sc','srw'],['hce','alu3','alu1','alu0','alr','aaw'],['hce','pchr','abw'],['hce','alu3','alu0','alr','pchw'],['tcclr']]
not_branch = [['pcce'],['tcclr']]

############ ADDRESSING MODES ################
#They do all required address calculations and get operands ready for actual operations.
#Inmediate:
in_am = [['pclr', 'adlw', 'pchra','adhe'],['pcce']]

#Zero page
zp_am = [['pclr', 'adlw', 'pchra','adhe'],['extr','adlw', 'adhe','pcce']]

#We enable hidden carry, so we can perform address calculations without disturbing the regular carry flag. HC works as usual.

#Zero page, X indexed
#          Prog Counter -> Addr regs;               Zpg addr -> ALU A, clear c;           RX -> ALU B;         zpg addr + X reg -> adl reg, 0->adh;
zp_x_am = [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','aaw','sc','fgs','pcce'],['hce','rxr','abw'],['hce','alu0','alu3','alr','adlw','adhe']]

#Zero page, Y indexed
#          Prog Counter -> Addr regs;               Zpg addr -> ALU A, clear c;           RY -> ALU B;         zpg addr + X reg -> adl reg, 0->adh;
zp_y_am = [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','aaw','sc','fgs','pcce'],['hce','ryr','abw'],['hce','alu0','alu3','alr','adlw','adhe']]

#Absolute
#         Prog Counter -> Addr regs         Addr Low -> ALU A;      Prog Counter -> Addr regs;      Memory -> addr reg high;  ALU A-> Addr low;
ab_am = [['pclr', 'adlw', 'pchra','adhe'],['extr','aaw', 'pcce'],['pclr', 'adlw', 'pchra','adhe'],['extr','adhe', 'adhw', 'pcce'],['alr', 'adlw']]

#Absolute, X indexed
#           Prog Counter -> Addr regs;              Addr Low -> ALU A,clear c;             RX -> ALU B;         Prog Counter -> Addr regs;              Memory -> addr high;          low byte + X reg -> adl reg, save c;           adh -> ALU A;        add0 adh(w. carry) -> adh;         
ab_x_am = [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','aaw','sc','fgs','pcce'],['hce','rxr', 'abw'],['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','adhe', 'adhw', 'pcce'], ['hce','alu0','alu3','sc','srw','alr','adlw'],['hce','adhr','aaw'],['hce','alu0','alu1','alu2','alr','adhe', 'adhw']]

#Absolute, Y indexed
#           Prog Counter -> Addr regs;              Addr Low -> ALU A,clear c;             RX -> ALU B;         Prog Counter -> Addr regs;              Memory -> addr high;          low byte + X reg -> adl reg, save c;           adh -> ALU A;        add0 adh(w. carry) -> adh;         
ab_y_am = [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','aaw','sc','fgs','pcce'],['hce','ryr','abw'],['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','adhe', 'adhw', 'pcce'], ['hce','alu0','alu3','sc','srw','alr','adlw'],['hce','adhr','aaw'],['hce','alu0','alu1','alu2','alr','adhe', 'adhw']]

#X, indirect
# Operand at address stored at zpg in (Zpg address + reg X)
#           Prog Counter -> Addr regs;              Zpg addr -> ALU A, clear c;            RX -> ALU B;        zpg addr + X reg -> adl.alu a , 0->adh;          LSB -> adh;                            SUM0 A ALU-> adl;                         adh -> ALU A;        0->adh, acc->ALU B;         MSB -> ADH;           ALU -> ADL; 
x_id_am = [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','aaw','sc','fgs','pcce'],['hce','rxr','abw'],['hce','alu0','alu3','alr','adlw','aaw','adhe'],['hce','extr','adhe', 'adhw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','adlw'],['hce','adhr','aaw'],['hce','adhe','rar','abw'],['hce','extr','adhe', 'adhw'],['hce','alr','adlw']]

#Indirect, Y
# Operand stored at [(addres stored at zpg address) + (reg Y)]
#           Prog Counter -> Addr regs;              Zpg ad->adl.aluA, 0->adh;                  Addr Low -> ADH;                       SUM0 A ALU-> ADL;                         ADH->ALU A;           RY->ALU B, 0->ADH;          Mem->ADH,clear c;                LSB+Y ALU -> ADL, save c;                      ADH->ALU A;          SUM0 ALU A->ADH  (Addr MSB w. carry);  
id_y_am = [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','adlw','aaw','adhe','pcce'],['hce','extr','adhe', 'adhw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','adlw'],['hce','adhr', 'aaw'],['hce','ryr','abw','adhe'],['hce','extr','adhe', 'adhw','sc','fgs'],['hce','alu0','alu3','alr','adlw','sc','srw'],['hce','adhr','aaw'],['hce','alu0','alu1','alu2','alr','adhe', 'adhw']]


################## MICROCODE ##################

microcode = {
    #################### REGISTER TRANSFER ####################
    'tax':
    [#RA->ALU A;    ALU->RX, mod N,Z
    [['rar','aaw'],['alr','rxw','sn','sz','srw'],['tcclr']],
    [['rar','aaw'],['alr','rxw','sn','sz','srw'],['tcclr']],
    ],
    'txa':
    [
    [['rxr','aaw'],['alr','raw','sn','sz','srw'],['tcclr']],
    [['rxr','aaw'],['alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'tya':
    [
    [['ryr','aaw'],['alr','raw','sn','sz','srw'],['tcclr']],
    [['ryr','aaw'],['alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'tay':
    [
    [['rar','aaw'],['alr','ryw','sn','sz','srw'],['tcclr']],
    [['rar','aaw'],['alr','ryw','sn','sz','srw'],['tcclr']],
    ],
    'tsx':
    [
    [['spr','aaw'],['alr','rxw','sn','sz','srw'],['tcclr']],
    [['spr','aaw'],['alr','rxw','sn','sz','srw'],['tcclr']],
    ],
    'txs':
    [
    [['rxr','spw'],['tcclr']],
    [['rxr','spw'],['tcclr']],
    ],

    ################## ALU ##########################
    #INX--------------------------------------------------------
    'inx':
    [# RX-> ALU A, set C;                  SUM0 ALU ->RX, mod n,z;
    [['hce','rxr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','rxw','sn','sz','srw'], ['tcclr']],
    [['hce','rxr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','rxw','sn','sz','srw'], ['tcclr']],
    ],

    #INY--------------------------------------------------------
    'iny':
    [# RY-> ALU A, set C;                  SUM0 ALU ->RY, mod n,z;
    [['hce','ryr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','ryw','sn','sz','srw'], ['tcclr']],
    [['hce','ryr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','ryw','sn','sz','srw'], ['tcclr']],
    ],

    #INC--------------------------------------------------------
    'inc_zp':
    [#         MEM -> ALU A,set C;                  SUM0 ALU ->MEM, mod n,z;
    zp_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    zp_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    ],
    'inc_zp_x':
    [#         MEM -> ALU A,set C;                  SUM0 ALU ->MEM, mod n,z;
    zp_x_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    zp_x_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    ],
    'inc_ab':
    [#         MEM -> ALU A,set C;                  SUM0 ALU ->MEM, mod n,z;
    ab_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    ab_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    ],
    'inc_ab_x':
    [#         MEM -> ALU A,set C;                  SUM0 ALU ->MEM, mod n,z;
    ab_x_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    ab_x_am + [['hce','extr','aaw','sc','fgs','fgv'],['hce','alu2','alu1','alu0','alr','extw','sn','sz','srw'], ['tcclr']],
    ],

    #DEX------------------------------------------------------
    'dex':
    [# RX-> ALU A, clear C;          SUB0 ALU ->RX, mod n,z;
    [['hce','rxr','aaw','sc','fgs'],['hce','alu3','alr','rxw','sn','sz','srw'], ['tcclr']],
    [['hce','rxr','aaw','sc','fgs'],['hce','alu3','alr','rxw','sn','sz','srw'], ['tcclr']],
    ],

    #DEY--------------------------------------------------------
    'dey':
    [# RY-> ALU A, clear C;          SUB0 ALU ->RY, mod n,z;
    [['hce','ryr','aaw','sc','fgs'],['hce','alu3','alr','ryw','sn','sz','srw'], ['tcclr']],
    [['hce','ryr','aaw','sc','fgs'],['hce','alu3','alr','ryw','sn','sz','srw'], ['tcclr']],
    ],

    #DEC--------------------------------------------------------
    'dec_zp':
    [#         MEM -> ALU A, clear C;                  SUB0 ALU ->MEM, mod n,z;
    zp_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    zp_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    ],
    'dec_zp_x':
    [#         MEM -> ALU A, clear C;                  SUB0 ALU ->MEM, mod n,z;
    zp_x_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    zp_x_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    ],
    'dec_ab':
    [#         MEM -> ALU A, clear C;                  SUB0 ALU ->MEM, mod n,z;
    ab_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    ab_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    ],
    'dec_ab_x':
    [#         MEM -> ALU A, clear C;                  SUB0 ALU ->MEM, mod n,z;
    ab_x_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    ab_x_am + [['hce','extr','aaw','sc','fgs'],['hce','alu3','alr','extw','sn','sz','srw'], ['tcclr']],
    ],


    #ORA-----------------------------------------------------------------------------------------------
    'ora_in':
    [
    in_am + [['extr','aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    in_am + [['extr','aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'ora_zp':
    [
    zp_am + [['extr','aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    zp_am + [['extr','aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'ora_zp_x':
    [#         mem -> ALU A;  acc -> ALU B;       A or B -> acc, mod. n and z flags;  
    zp_x_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    zp_x_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'ora_ab':
    [#        Memory-> ALU A; acc -> ALU B; Operation->accumulator, mod. n and z flags;
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'ora_ab_x':
    [#        mem -> ALU A;  acc -> ALU B;       A or B -> acc, mod. n and z flags; 
    ab_x_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ab_x_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'ora_ab_y':
    [#          mem -> ALU A;  acc -> ALU B;     A or B -> acc, mod. n and z flags;  
    ab_y_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ab_y_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'ora_x_id': 
    [#         mem -> ALU A;  acc -> ALU B;       A or B -> acc, mod. n and z flags; 
    x_id_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    x_id_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'ora_id_y': 
    [#          mem -> ALU A;  acc -> ALU B;      A or B -> acc, mod. n and z flags;  
    id_y_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    id_y_am + [['extr', 'aaw'],['rar','abw'],['alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],

    #AND-----------------------------------------------------------------------------------------------
    'and_in':
    [
    in_am + [['extr','aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    in_am + [['extr','aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'and_zp':
    [
    zp_am + [['extr','aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    zp_am + [['extr','aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'and_zp_x':
    [#         mem -> ALU A;  acc -> ALU B;       A and B -> acc, mod. n and z flags;  
    zp_x_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    zp_x_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'and_ab':
    [#        Memory-> ALU A; acc -> ALU B; Operation->accumulator, mod. n and z flags;
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'and_ab_x':
    [#        mem -> ALU A;  acc -> ALU B;       A and B -> acc, mod. n and z flags; 
    ab_x_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ab_x_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'and_ab_y':
    [#          mem -> ALU A;  acc -> ALU B;     A and B -> acc, mod. n and z flags;  
    ab_y_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ab_y_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'and_x_id': 
    [#         mem -> ALU A;  acc -> ALU B;       A and B -> acc, mod. n and z flags; 
    x_id_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    x_id_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'and_id_y': 
    [#          mem -> ALU A;  acc -> ALU B;      A and B -> acc, mod. n and z flags;  
    id_y_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    id_y_am + [['extr', 'aaw'],['rar','abw'],['alu2','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    
    #EOR-----------------------------------------------------------------------------------------------
    'eor_in':
    [
    in_am + [['extr','aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    in_am + [['extr','aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'eor_zp':
    [
    zp_am + [['extr','aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    zp_am + [['extr','aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'eor_zp_x':
    [#         mem -> ALU A;  acc -> ALU B;       A eor B -> acc, mod. n eor z flags;  
    zp_x_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    zp_x_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'eor_ab':
    [#        Memory-> ALU A; acc -> ALU B; Operation->accumulator, mod. n eor z flags;
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'eor_ab_x':
    [#        mem -> ALU A;  acc -> ALU B;       A eor B -> acc, mod. n eor z flags; 
    ab_x_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ab_x_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'eor_ab_y':
    [#          mem -> ALU A;  acc -> ALU B;     A eor B -> acc, mod. n eor z flags;  
    ab_y_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ab_y_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'eor_x_id': 
    [#         mem -> ALU A;  acc -> ALU B;       A eor B -> acc, mod. n eor z flags; 
    x_id_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    x_id_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],
    'eor_id_y': 
    [#          mem -> ALU A;  acc -> ALU B;      A eor B -> acc, mod. n eor z flags;  
    id_y_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    id_y_am + [['extr', 'aaw'],['rar','abw'],['alu0','alu1','alr','raw','sn','sz','srw'],['tcclr']],
    ],


    #ADC-----------------------------------------------------------------------------------------------
    'adc_in':
    [#         mem -> ALU B;  acc -> ALU A;       A + B -> acc, mod. n,v,z,c flags;
    in_am + [['extr','abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    in_am + [['extr','abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'adc_zp':
    [#         mem -> ALU B;  acc -> ALU A;       A + B -> acc, mod. n,v,z,c flags;
    zp_am + [['extr','abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    zp_am + [['extr','abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'adc_zp_x':
    [#         mem -> ALU B;  acc -> ALU A;       A + B -> acc, mod. n,v,z,c flags;  
    zp_x_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    zp_x_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'adc_ab':
    [#        Memory-> ALU A; acc -> ALU A; Operation->accumulator, mod. n,v,z,c flags;
    ab_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ab_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'adc_ab_x':
    [#        mem -> ALU B;  acc -> ALU A;       A + B -> acc, mod. n,v,z,c flags; 
    ab_x_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ab_x_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'adc_ab_y':
    [#          mem -> ALU B;  acc -> ALU A;     A + B -> acc, mod. n,v,z,c flags;  
    ab_y_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ab_y_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'adc_x_id': 
    [#         mem -> ALU B;  acc -> ALU A;       A + B -> acc, mod. n,v,z,c flags; 
    x_id_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    x_id_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'adc_id_y': 
    [#          mem -> ALU B;  acc -> ALU A;      A + B -> acc, mod. n,v,z,c flags;  
    id_y_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    id_y_am + [['extr', 'abw'],['rar','aaw'],['alu0','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],

    #SBC-----------------------------------------------------------------------------------------------
    'sbc_in':
    [#         mem -> ALU B;  acc -> ALU A;       A - B -> acc, mod. n,v,z,c flags;  
    in_am + [['extr','abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    in_am + [['extr','abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'sbc_zp':
    [#         mem -> ALU B;  acc -> ALU A;       A - B -> acc, mod. n,v,z,c flags;  
    zp_am + [['extr','abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    zp_am + [['extr','abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'sbc_zp_x':
    [#         mem -> ALU B;  acc -> ALU A;       A - B -> acc, mod. n,v,z,c flags;  
    zp_x_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    zp_x_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'sbc_ab':
    [#        mem -> ALU B;  acc -> ALU A;       A - B -> acc, mod. n,v,z,c flags; 
    ab_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ab_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'sbc_ab_x':
    [#        mem -> ALU B;  acc -> ALU A;       A - B -> acc, mod. n,v,z,c flags; 
    ab_x_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ab_x_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'sbc_ab_y':
    [#          mem -> ALU B;  acc -> ALU A;     A - B -> acc, mod. n,v,z,c flags;  
    ab_y_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ab_y_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'sbc_x_id': 
    [#         mem -> ALU B;  acc -> ALU A;       A - B -> acc, mod. n,v,z,c flags; 
    x_id_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    x_id_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],
    'sbc_id_y': 
    [#          mem -> ALU B;  acc -> ALU A;      A - B -> acc, mod. n,v,z,c flags;  
    id_y_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    id_y_am + [['extr', 'abw'],['rar','aaw'],['alu1','alu3','alr','raw','sn','sv','sz','sc','srw'],['tcclr']],
    ],


    #BIT------------------------------------------------------------------------
    'bit_zp':
    [#         MEM->ALU A;    ACC-> ALU B; (BIT)ALU->Nowhere, mod NVZ flags;
    zp_am + [['extr', 'aaw'],['rar','abw'],['alu3','alu2','alu3','sn','sv','sz','srw'],['tcclr']],
    zp_am + [['extr', 'aaw'],['rar','abw'],['alu3','alu2','alu3','sn','sv','sz','srw'],['tcclr']],
    ],
    'bit_ab':
    [#         MEM->ALU A;    ACC-> ALU B; (BIT)ALU->Nowhere, mod NVZ flags;
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu3','alu2','alu3','sn','sv','sz','srw'],['tcclr']],
    ab_am + [['extr', 'aaw'],['rar','abw'],['alu3','alu2','alu3','sn','sv','sz','srw'],['tcclr']],
    ],

    #ASL-----------------------------------------------------------------------------------------------
    'asl_a':
    [#  ACC -> ALU A, clear c.;     ROL w. C = 0, same as ASL.-> ACC Mod ZCN;
    [['rar','aaw','sc','fgs'],['alu0','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    [['rar','aaw','sc','fgs'],['alu0','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    ],
	'asl_zp':
    [#         MEM -> ALU A, clear c.     ROL w. C = 0, same as ASL.-> MEM Mod ZCN
    zp_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'asl_zp_x':
    [#         MEM -> ALU A, clear c.     ROL w. C = 0, same as ASL.-> MEM Mod ZCN
    zp_x_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_x_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'asl_ab':
    [#         MEM -> ALU A, clear c.     ROL w. C = 0, same as ASL.-> MEM Mod ZCN
    ab_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'asl_ab_x':
    [#         MEM -> ALU A, clear c.     ROL w. C = 0, same as ASL.-> MEM Mod ZCN
    ab_x_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_x_am + [['extr','aaw','sc','fgs'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],

    #LSR-----------------------------------------------------------------------------------------------
    'lsr_a':
    [#  ACC -> ALU A, clear c.;     ROR w. C = 0, same as LSR.-> ACC Mod ZCN;
    [['rar','aaw','sc','fgs'],['alu1','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    [['rar','aaw','sc','fgs'],['alu1','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    ],
	'lsr_zp':
    [#         MEM -> ALU A, clear c.     ROR w. C = 0, same as LSR.-> MEM Mod ZCN
    zp_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'lsr_zp_x':
    [#         MEM -> ALU A, clear c.     ROR w. C = 0, same as LSR.-> MEM Mod ZCN
    zp_x_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_x_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'lsr_ab':
    [#         MEM -> ALU A, clear c.     ROR w. C = 0, same as LSR.-> MEM Mod ZCN
    ab_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'lsr_ab_x':
    [#         MEM -> ALU A, clear c.     ROR w. C = 0, same as LSR.-> MEM Mod ZCN
    ab_x_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_x_am + [['extr','aaw','sc','fgs'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
   
    #ROL-----------------------------------------------------------------------------------------------
    'rol_a':
    [#  ACC -> ALU A    ROL -> ACC Mod ZCN;
    [['rar','aaw'],['alu0','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    [['rar','aaw'],['alu0','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    ],
	'rol_zp':
    [#        MEM -> ALU A      ROL -> MEM Mod ZCN
    zp_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'rol_zp_x':
    [#         MEM -> ALU A     ROL -> MEM Mod ZCN
    zp_x_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_x_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'rol_ab':
    [#         MEM -> ALU A     ROL -> MEM Mod ZCN
    ab_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'rol_ab_x':
    [#         MEM -> ALU A     ROL -> MEM Mod ZCN
    ab_x_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_x_am + [['extr','aaw'],['alu0','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],

    #ROR-----------------------------------------------------------------------------------------------
    'ror_a':
    [#  ACC -> ALU A    ROR -> ACC Mod ZCN;
    [['rar','aaw'],['alu1','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    [['rar','aaw'],['alu1','alu2','alr','raw','sz','sc','sn','srw'],['tcclr']],
    ],
	'ror_zp':
    [#        MEM -> ALU A      ROR -> MEM Mod ZCN
    zp_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'ror_zp_x':
    [#         MEM -> ALU A     ROR -> MEM Mod ZCN
    zp_x_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    zp_x_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'ror_ab':
    [#         MEM -> ALU A     ROR -> MEM Mod ZCN
    ab_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],
    'ror_ab_x':
    [#         MEM -> ALU A     ROR -> MEM Mod ZCN
    ab_x_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ab_x_am + [['extr','aaw'],['alu1','alu2','alr','extw','sz','sc','sn','srw'],['tcclr']],
    ],


    ############################# LOAD #################################
    #LDA----------------------------------
    'lda_in':
    [
        in_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        in_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],
    'lda_zp':
    [
        zp_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        zp_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],
    'lda_zp_x':
    [
        zp_x_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        zp_x_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],
    'lda_ab':
    [
        ab_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        ab_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],
    'lda_ab_x':
    [
        ab_x_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        ab_x_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],
    'lda_ab_y':
    [
        ab_y_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        ab_y_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],
    'lda_x_id':
    [
        x_id_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        x_id_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],
    'lda_id_y':
    [
        id_y_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
        id_y_am + [['extr','aaw'],['alr','raw','sz','sn','srw'],['tcclr']],
    ],

    #LDX----------------------------------
    'ldx_in':
    [
        in_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
        in_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
    ],
    'ldx_zp':
    [
        zp_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
        zp_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
    ],
    'ldx_zp_y':
    [
        zp_y_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
        zp_y_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
    ],
    'ldx_ab':
    [
        ab_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
        ab_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
    ],
    'ldx_ab_y':
    [
        ab_y_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
        ab_y_am + [['extr','aaw'],['alr','rxw','sz','sn','srw'],['tcclr']],
    ],

    #LDY----------------------------------
    'ldy_in':
    [
        in_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
        in_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
    ],
    'ldy_zp':
    [
        zp_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
        zp_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
    ],
    'ldy_zp_x':
    [
        zp_x_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
        zp_x_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
    ],
    'ldy_ab':
    [
        ab_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
        ab_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
    ],
    'ldy_ab_x':
    [
        ab_x_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
        ab_x_am + [['extr','aaw'],['alr','ryw','sz','sn','srw'],['tcclr']],
    ],
    ########################## COMPARE ############################
    #CMP---------------------------------------------------------
    'cmp_in':
    [#            MEM->ALU B, set c;              ACC -> ALU A;   (SUB) ALU-> Nowhere, mod. ZCN;
        in_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        in_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],    
	'cmp_zp':
    [
        zp_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        zp_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
    'cmp_zp_x':
    [
        zp_x_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        zp_x_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
    'cmp_ab':
    [
        ab_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        ab_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
    'cmp_ab_x':
    [
        ab_x_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        ab_x_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
    'cmp_ab_y':
    [
        ab_y_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        ab_y_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
    'cmp_x_id':
    [
        x_id_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        x_id_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
    'cmp_id_y':
    [
        id_y_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        id_y_am + [['extr','abw','sc','fgs','fgv'],['rar','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],

    #CPX--------------------------------------------------------------
    'cpx_in':
    [#            MEM->ALU B, set c;              RX -> ALU A;   (SUB) ALU-> Nowhere, mod. ZCN;
        in_am + [['extr','abw','sc','fgs','fgv'],['rxr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        in_am + [['extr','abw','sc','fgs','fgv'],['rxr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],    
	'cpx_zp':
    [
        zp_am + [['extr','abw','sc','fgs','fgv'],['rxr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        zp_am + [['extr','abw','sc','fgs','fgv'],['rxr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
    'cpx_ab':
    [
        ab_am + [['extr','abw','sc','fgs','fgv'],['rxr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        ab_am + [['extr','abw','sc','fgs','fgv'],['rxr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],

    #CPY-----------------------------------------------------------
    'cpy_in':
    [#            MEM->ALU B, set c;              RY -> ALU A;   (SUB) ALU-> Nowhere, mod. ZCN;
        in_am + [['extr','abw','sc','fgs','fgv'],['ryr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        in_am + [['extr','abw','sc','fgs','fgv'],['ryr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],    
	'cpy_zp':
    [
        zp_am + [['extr','abw','sc','fgs','fgv'],['ryr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        zp_am + [['extr','abw','sc','fgs','fgv'],['ryr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ], 
    'cpy_ab':
    [
        ab_am + [['extr','abw','sc','fgs','fgv'],['ryr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
        ab_am + [['extr','abw','sc','fgs','fgv'],['ryr','aaw'],['alu1','alu3','sz','sc','sn','srw'],['tcclr']],
    ],
   

    ########################## STORE ##############################
    #STA------------------------------------------------------
    'sta_zp':
    [
        zp_am + [['extw','rar'],['tcclr']],
        zp_am + [['extw','rar'],['tcclr']],
    ],
    'sta_zp_x':
    [
        zp_x_am + [['extw','rar'],['tcclr']],
        zp_x_am + [['extw','rar'],['tcclr']],
    ],
    'sta_ab':
    [
        ab_am + [['extw','rar'],['tcclr']],
        ab_am + [['extw','rar'],['tcclr']],
    ],
    'sta_ab_x':
    [
        ab_x_am + [['extw','rar'],['tcclr']],
        ab_x_am + [['extw','rar'],['tcclr']],
    ],
    'sta_ab_y':
    [
        ab_y_am + [['extw','rar'],['tcclr']],
        ab_y_am + [['extw','rar'],['tcclr']],
    ],
    'sta_x_id':
    [
        x_id_am + [['extw','rar'],['tcclr']],
        x_id_am + [['extw','rar'],['tcclr']],
    ],
    'sta_id_y':
    [
        id_y_am + [['extw','rar'],['tcclr']],
        id_y_am + [['extw','rar'],['tcclr']],
    ],
    #STX--------------------------------------------------------------
    'stx_zp':
    [
        zp_am + [['extw','rxr'],['tcclr']],
        zp_am + [['extw','rxr'],['tcclr']],
    ],
    'stx_zp_y':
    [
        zp_y_am + [['extw','rxr'],['tcclr']],
        zp_y_am + [['extw','rxr'],['tcclr']],
    ],
    'stx_ab':
    [
        ab_am + [['extw','rxr'],['tcclr']],
        ab_am + [['extw','rxr'],['tcclr']],
    ],
    
    #STY---------------------------------------------------------------
    'sty_zp':
    [
        zp_am + [['extw','ryr'],['tcclr']],
        zp_am + [['extw','ryr'],['tcclr']],
    ],
    'sty_zp_x':
    [
        zp_x_am + [['extw','ryr'],['tcclr']],
        zp_x_am + [['extw','ryr'],['tcclr']],
    ],
    'sty_ab':
    [
        ab_am + [['extw','ryr'],['tcclr']],
        ab_am + [['extw','ryr'],['tcclr']],
    ],
    
    ########################## STACK ###########################
    'pha':
    [#  SP->ADL.ALU A,0x1->ADH;                     A->MEM, clear c;                (SUB0)ALU -> SP
        [['hce','spr','adlw','aaw','cg0','adhe'],['hce','rar','extw','sc','fgs'],['hce','alu3','alr','spw'],['tcclr']],
        [['hce','spr','adlw','aaw','cg0','adhe'],['hce','rar','extw','sc','fgs'],['hce','alu3','alr','spw'],['tcclr']],
    ],
    'php':
    [#  SP->ADL.ALU A,0x1->ADH;                     SR->MEM, clear c;                (SUB0)ALU -> SP
        [['hce','spr','adlw','aaw','cg0','adhe'],['hce','srr','extw','sc','fgs'],['hce','alu3','alr','spw'],['tcclr']],
        [['hce','spr','adlw','aaw','cg0','adhe'],['hce','srr','extw','sc','fgs'],['hce','alu3','alr','spw'],['tcclr']],
    ],
    'pla':
    [#   SP -> ALU A,set C;                    (SUM0)ALU -> SP;                         SP->ADL,0x1->ADH;                   MEM->ALU A;         ALU -> ACC, mod z,n;                
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','aaw'],['hce','alr','raw','sz','sn','srw'],['tcclr']],
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','aaw'],['hce','alr','raw','sz','sn','srw'],['tcclr']],
    ],

    'plp':
    [#      SP->ALU A, setc;                   (SUM0)ALU->SP;                           SP->ADL,0x1->ADH;                    MEM->SR;                   
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','srw'],['tcclr']],
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','srw'],['tcclr']],
    ],

    ######################### CONDITIONAL BRANCHES ##################################
    #BCC, branch on carry clear --------------------------------------------------------------------------
    'bcc':
    [#   Select c, P.C. -> Addr regs;           
        [['sc','pclr', 'adlw','pchra','adhe']] + branch,
        [['sc','pclr', 'adlw','pchra','adhe']] + not_branch, #If C set, do nothing
    ],
    #BCS, branch on carry set--------------------------------------------------------------------------
    'bcs':
    [#   Select n, P.C. -> Addr regs;           
        [['sc','pclr', 'adlw','pchra','adhe']] + not_branch, #If C clear, do nothing
        [['sc','pclr', 'adlw','pchra','adhe']] + branch, 
    ],
    #BEQ, branch if equal, z flag set ---------------------------------------------------------------------
    'beq':
    [#   Select z, P.C. -> Addr regs;           
        [['sz','pclr', 'adlw','pchra','adhe']] + not_branch, #If Z clear, do nothing
        [['sz','pclr', 'adlw','pchra','adhe']] + branch, 
    ],
  
    #BMI, branch if minus, n flag set---------------------------------------------------------------------
    'bmi':
    [#   Select n, P.C. -> Addr regs;           
        [['sn','pclr', 'adlw','pchra','adhe']] + not_branch, #If n clear, do nothing
        [['sn','pclr', 'adlw','pchra','adhe']] + branch, 
    ],

    #BNE, branch if not equal, z flag clear --------------------------------------------------------------
    'bne':
    [#   Select z, P.C. -> Addr regs;           
        [['sz','pclr', 'adlw','pchra','adhe']] + branch, 
        [['sz','pclr', 'adlw','pchra','adhe']] + not_branch, #If Z set, do nothing
    ],

    #BPL, branch on not Negative--------------------------------------------------------------------------
    'bpl':
    [#   Select n, P.C. -> Addr regs;           
        [['sn','pclr', 'adlw','pchra','adhe']] + branch,
        [['sn','pclr', 'adlw','pchra','adhe']] + not_branch, #If N set, do nothing
    ],

    #BVC, branch if v clear --------------------------------------------------------------
    'bvc':
    [#   Select v, P.C. -> Addr regs;           
        [['sv','pclr', 'adlw','pchra','adhe']] + branch, 
        [['sv','pclr', 'adlw','pchra','adhe']] + not_branch, #If V set, do nothing
    ],

    #BVS, branch if v set  --------------------------------------------------------------
    'bvs':
    [#   Select v, P.C. -> Addr regs;           
        [['sv','pclr', 'adlw','pchra','adhe']] + not_branch, #If V clear, do nothing
        [['sv','pclr', 'adlw','pchra','adhe']] + branch,
    ],


    #################### SET/CLEAR FLAGS ########################
    'clc':
    [#      Clear C                  
        [['sc','fgs'],['tcclr']],
        [['sc','fgs'],['tcclr']],
    ],
    'sec':
    [#      Set C                  
        [['sc','fgs','fgv'],['tcclr']],
        [['sc','fgs','fgv'],['tcclr']],
    ],
    'cli':
    [#      Clear I                   
        [['si','fgs'],['tcclr']],
        [['si','fgs'],['tcclr']],
    ],
    'sei':
    [#      Set I                   
        [['si','fgs','fgv'],['tcclr']],
        [['si','fgs','fgv'],['tcclr']],
    ],
    'clv':
    [#      Clear V                   
        [['sv','fgs'],['tcclr']],
        [['sv','fgs'],['tcclr']],
    ],
    ########################### MISC ##########################
    #RESET--------------------------------------------------------------------------------
    'rst': #RESET sequence
    [
        [['cg2','adlw','adhe'], ['extr', 'pclw'], ['cg2','cg0','adlw','adhe'], ['extr', 'pchw'], ['tcclr']],
        [['cg2','adlw','adhe'], ['extr', 'pclw'], ['cg2','cg0','adlw','adhe'], ['extr', 'pchw'], ['tcclr']],
    ],
    #NMI--------------------------------------------------------------------------------
    'nmi':
    [#  SP->ADL.ALU A,0x1->ADH;                    PCH->MEM, clear c;               (SUB0)ALU -> SP;          SP->ADL.ALU A,0x1->ADH;                  PCL->MEM;             (SUB0)ALU -> SP;            SP->ADL.ALU A,0x1->ADH;                  SR->MEM;             (SUB0)ALU -> SP;          FFFA -> Address regs.;   IRQL->PCL;       FFFB -> Address regs.;          IRQL->PCL, set i, reset nmi flip-flop; 
        [['hce','spr','adlw','aaw','cg0','adhe'],['hce','pchr','extw','sc','fgs'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','pclr','extw'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','srr','extw'],['hce','alu3','alr','spw'],['cg1','adlw','adhe'], ['extr', 'pclw'], ['cg1','cg0','adlw','adhe'], ['extr', 'pchw','si','fgs','fgv','nmi'],['tcclr']],
        [['hce','spr','adlw','aaw','cg0','adhe'],['hce','pchr','extw','sc','fgs'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','pclr','extw'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','srr','extw'],['hce','alu3','alr','spw'],['cg1','adlw','adhe'], ['extr', 'pclw'], ['cg1','cg0','adlw','adhe'], ['extr', 'pchw','si','fgs','fgv','nmi'],['tcclr']],
    ],
    #NOP-----------------------------------------------------------------------------------
    'nop':
    [
    [['tcclr']],
    [['tcclr']],
    ],
    
    ######################## JUMP/BRK ##########################

    #BRK------------------------------------------------------------------------------------
    'brk': 
    [#  SP->ADL.ALU A,0x1->ADH, PC++ (padding byte);      PCH->MEM, clear c;               (SUB0)ALU -> SP;          SP->ADL.ALU A,0x1->ADH;                  PCL->MEM;             (SUB0)ALU -> SP;            SP->ADL.ALU A,0x1->ADH;                  SR->MEM;             (SUB0)ALU -> SP;          FFFE -> Address regs.;         IRQL->PCL;       FFFF -> Address regs.;              IRQL->PCL, set i; (some discrepancy here, but must be this way, so it does not get stuck)
        [['hce','spr','adlw','aaw','cg0','adhe','pcce'],['hce','pchr','extw','sc','fgs'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','pclr','extw'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','srr','extw'],['hce','alu3','alr','spw'],['cg2','cg1','adlw','adhe'], ['extr', 'pclw'], ['cg2','cg1','cg0','adlw','adhe'], ['extr', 'pchw','si','fgs','fgv'],['tcclr']],
        [['hce','spr','adlw','aaw','cg0','adhe','pcce'],['hce','pchr','extw','sc','fgs'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','pclr','extw'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','srr','extw'],['hce','alu3','alr','spw'],['cg2','cg1','adlw','adhe'], ['extr', 'pclw'], ['cg2','cg1','cg0','adlw','adhe'], ['extr', 'pchw','si','fgs','fgv'],['tcclr']],
    ],

    #RTI-------------------------------------------------------------------------------------------------
    'rti':
    [#      SP->ALU A, setc;                   (SUM0)ALU->SP;                           SP->ADL,0x1->ADH;                    MEM->SR;             SP->ALU A, set c;                  (SUM0)ALU->SP;                           SP->ADL,0x1->ADH;                    MEM->PCL;             SP->ALU A, set c;                  (SUM0)ALU->SP;                           SP->ADL,0x1->ADH;                    MEM->PCH;                        
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','srw'],['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pclw'],['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pchw'],['tcclr']],
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','srw'],['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pclw'],['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pchw'],['tcclr']],
    ],
    
    #JSR------------------------------------------------------------------------------------------------- 
    'jsr':
    [#    Prog Counter -> Addr regs;              MEM -> ALU B;        SP->ADL.ALU A,0x1->ADH, PC ++;                   PCH -> MEM, clear c;            (SUB0) ALU -> SP;         SP->ADL.ALU A,0x1->ADH;                    PCL -> MEM;           SP->ALU A;          (SUB0) ALU -> SP;          Prog Counter -> Addr regs;              MEM ->PCH;    0-> A;  (SUM) ALU-> PCL (ALUB ->PCL);
        [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','abw'],['hce','spr','adlw','aaw','cg0','adhe','pcce'],['hce','pchr','extw','sc','fgs'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','pclr','extw'],['hce','spr','aaw'],['hce','alu3','alr','spw'],['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','pchw'],['hce','aaw'],['hce','alu0','alu3','alr','pclw'],['tcclr']],
        [['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','abw'],['hce','spr','adlw','aaw','cg0','adhe','pcce'],['hce','pchr','extw','sc','fgs'],['hce','alu3','alr','spw'],['hce','spr','adlw','aaw','cg0','adhe'],['hce','pclr','extw'],['hce','spr','aaw'],['hce','alu3','alr','spw'],['hce','pclr', 'adlw', 'pchra','adhe'],['hce','extr','pchw'],['hce','aaw'],['hce','alu0','alu3','alr','pclw'],['tcclr']],
    ],

    #RTS-------------------------------------------------------------------------------------------------
    'rts':
    [#      SP->ALU A, setc;                   (SUM0)ALU->SP;                           SP->ADL,0x1->ADH;                    MEM->PCL;             SP->ALU A;        (SUM0)ALU->SP;                           SP->ADL,0x1->ADH;                    MEM->PCH;           PC++        
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pclw'],['hce','spr','aaw'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pchw'],['pcce'],['tcclr']],
        [['hce','spr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pclw'],['hce','spr','aaw'],['hce','alu0','alu1','alu2','alr','spw'],['hce','spr','adlw','cg0','adhe'],['hce','extr','pchw'],['pcce'],['tcclr']],
    ],

    #JUMP------------------------------------------------------------------------------------------------
    #We don't use regular absolute addresing here because we want to jump to that address, not the operand stored there.
    'jmp_ab':
    [#PC->Addr. Regs.;                   MEM-ALU A, PC++;      PC->Addr. Regs.;                 MEM->PCH;       ALU A-> PCL;
    [['pclr', 'adlw', 'pchra','adhe'],['extr','aaw','pcce'],['pclr', 'adlw', 'pchra','adhe'],['extr','pchw'],['alr','pclw'],['tcclr']],
    [['pclr', 'adlw', 'pchra','adhe'],['extr','aaw','pcce'],['pclr', 'adlw', 'pchra','adhe'],['extr','pchw'],['alr','pclw'],['tcclr']],
    ],
    #JUMP uses indirect addressing. Here we use absolute, which directs us to the lower byte of the taget address.
    'jmp_id':
    [#         MEM->PCL;           ADL-> ALU A, set C;                    (SUM0)ALU -> ADL, mod C;                            ADH-> ALU A;    (SUM0)ALU->ADH;                            MEM -> PCH;  
    ab_am + [['hce','extr','pclw'],['hce','adlr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','adlw','sc','srw'],['hce','adhr','aaw'],['hce','alu0','alu1','alu2','alr','adhe', 'adhw'],['hce','extr','pchw'],['tcclr']],
    ab_am + [['hce','extr','pclw'],['hce','adlr','aaw','sc','fgs','fgv'],['hce','alu0','alu1','alu2','alr','adlw','sc','srw'],['hce','adhr','aaw'],['hce','alu0','alu1','alu2','alr','adhe', 'adhw'],['hce','extr','pchw'],['tcclr']],
    ],
}

    




############## FILE HANDLING & DATA FORMATTING #####################

def formatData(out, rom):
        for line in activeLow:
            out^=bits[line]
        return str(hex((out>>(rom*8)) & 0b11111111))[2:].zfill(2)


def writeContents(file, rom):
    print('Writing ROM ' + str(rom) + '...')
    file.write('v2.0 raw\n')
    file.write('\n#ROM ' + str(rom) + ' CONTENTS\n')
    for opcode in range(256):
        data = ''
        if opcode in definedOpcodes:
            instruction = microcode[definedOpcodes[opcode]]
        else:
            instruction = microcode['nop']
        file.write('\n#'+ str(hex(opcode))+'---------------------------------\n')
        for case in instruction:
            #Fetch sequence- same for every instruction
            for time in fetchSequence:
                out = 0b0
                for ctrlLine in time:
                    out = out | bits[ctrlLine]
                data += formatData(out, rom) + ' '
            #Instruction specific 
            i = 0
            for time in case:
                i+=1
                for ctrlLine in time:
                    out = out | bits[ctrlLine]
                data += formatData(out, rom) + ' '
            for i in range(16-len(case)-len(fetchSequence)):
                data += formatData(0, rom) + ' '
            data += '  '
        file.write(data)



#OPEN TEXT FILES
rom0 = open("_ROM0.txt","w") 
rom1 = open("_ROM1.txt","w") 
rom2 = open("_ROM2.txt","w") 
rom3 = open("_ROM3.txt","w") 
rom4 = open("_ROM4.txt","w")
rom5 = open("_ROM5.txt","w") 

romFiles = [rom0,rom1,rom2,rom3,rom4,rom5]

#WRITE CONTENTS
for rom in range(6):
    writeContents(romFiles[rom], rom)

#CLOSE FILES
for i in range(6):
    romFiles[i].close()

print('Done')
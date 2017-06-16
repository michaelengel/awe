`define ENS #1 
`define NCLK @(posedge sysclk)

`define PC       pc
`define OPA      ((ir2[19:16]==15) ? pc : r[ir2[19:16]])
`define OPB      (ir2[25] ? rotimm(ir2[11:0]) : ((ir2[3:0]==15) ? pc : r[ir2[3:0]]))
//`define OFFSET4  {ir2[23],ir2[23],ir2[23],ir2[23],ir2[23],ir2[23],ir2[23:0],2'b00}
`define OFFSET4 {{ir2[23]?8'hff:8'h00},ir2[23:0],2'b00}
`define RD       r[dest]
`define P ir2[24]
`define U ir2[23]
`define B ir2[22]
`define W ir2[21]
`define L ir2[20]

// Processor Status Register bits
`define PSR_N psr[31]
`define PSR_Z psr[30]
`define PSR_C psr[29]
`define PSR_V psr[28]
`define PSR_Q psr[27]
`define PSR_I psr[7]    // IRQ enable
`define PSR_F psr[6]    // FIQ enable
`define PSR_T psr[5]    // Thumb mode
`define PSR_M psr[4:0]  // Processor mode
                        // 0x10 = User, 0x11 = FIQ, 0x12 = IRQ, 0x13 = Abort,
                        // 0x17 = Supervisor, 0x1b = Undef. insn, 0x1f = System

`define SERIAL_MUL
`define VECT_UNIMP1 120
`define VECT_EXTINT 120


//module  arm7_system(cont,sysclk);
// module  arm7_system(cont,sysclk,x,y,ir1,ir2);
module  arm7_system(cont,sysclk, ir1, ir2);
  input cont;
  input sysclk;
//  input x;
//  input y;
  output[31:0] ir1;
  output[31:0] ir2;
  wire cont;
  wire sysclk;

  reg supervisor;   //6/26/99
  wire intreq;      //7/1/99
  reg [31:0] ir1,ir2,psr;
  reg halt;
  reg [31:0] m[127:0];
  reg [31:0] r[15:0];
  reg [31:0] pc;  // 5/8/99
  reg [31:0] t;   // 5/9/99
  reg l;          // 5/9/99
  reg b;
  reg [3:0] dest; // 5/9/99
  reg [31:0] pc2; //5/10/99
  reg [4:0] cnt;  //7/22/99  


  function integer rotimm;  
    input [31:0] ir;
    begin
        rotimm =0;
        if (ir[11:8]==4'h0) rotimm = ir[7:0];
        if (ir[11:8]==4'h1) rotimm = (ir[1:0]<<30)|ir[7:2]; //6/9/99
        if (ir[11:8]==4'h2) rotimm = (ir[3:0]<<28)|ir[7:4]; //6/9/99
        if (ir[11:8]==4'h3) rotimm = (ir[5:0]<<26)|ir[7:6]; //6/9/99
        if (ir[11:8]==4'h4) rotimm = ir[7:0]<<24;
        if (ir[11:8]==4'h5) rotimm = ir[7:0]<<22;
        if (ir[11:8]==4'h6) rotimm = ir[7:0]<<20;
        if (ir[11:8]==4'h7) rotimm = ir[7:0]<<18;
        if (ir[11:8]==4'h8) rotimm = ir[7:0]<<16;
        if (ir[11:8]==4'h9) rotimm = ir[7:0]<<14;
        if (ir[11:8]==4'ha) rotimm = ir[7:0]<<12;
        if (ir[11:8]==4'hb) rotimm = ir[7:0]<<10;
        if (ir[11:8]==4'hc) rotimm = ir[7:0]<<8;
        if (ir[11:8]==4'hd) rotimm = ir[7:0]<<6;
        if (ir[11:8]==4'he) rotimm = ir[7:0]<<4;
        if (ir[11:8]==4'hf) rotimm = ir[7:0]<<2;
    end      
  endfunction      

  function integer condx;          
    input [31:0] condtype,psr;
    begin
        condx = 0;      
        if (condtype==4'h0) condx =   psr[30]; //`PSR_Z; 
        if (condtype==4'h1) condx = ! psr[30]; //`PSR_Z;
        if (condtype==4'h2) condx =   psr[29]; //`PSR_C;
        if (condtype==4'h3) condx = ! psr[29]; //`PSR_C;
        if (condtype==4'h4) condx =   psr[31]; //`PSR_N;  // 5/28/99 psr[31];
        if (condtype==4'h5) condx = ! psr[31]; //`PSR_N; // 5/28/99 !psr[31];
        if (condtype==4'h6) condx =   psr[28]; //`PSR_V;
        if (condtype==4'h7) condx = ! psr[28]; //`PSR_V;
        if (condtype==4'h8) condx =  (psr[29] && (!psr[30])); //(`PSR_C && (!`PSR_Z));
        if (condtype==4'h9) condx = !(psr[29] && (!psr[30])); //(`PSR_C && (!`PSR_Z));
        if (condtype==4'ha) condx =  (psr[31] == psr[28] );   //( `PSR_N == `PSR_V ); 
        if (condtype==4'hb) condx = !(psr[31] == psr[28] );   //( `PSR_N == `PSR_V );
        if (condtype==4'hc) condx =  ((psr[31]== psr[28]) && (!psr[30])); //((`PSR_N == `PSR_V) && (!`PSR_Z));
        if (condtype==4'hd) condx = !((psr[31]== psr[28]) && (!psr[30])); //((`PSR_N == `PSR_V) && (!`PSR_Z));
        if (condtype==4'he) condx = 1;
        if (condtype==4'hf) condx = 0;
    end
  endfunction

  function integer dp33; 
    input [31:0] opcode,opa,opb,psr;
    begin
      if      (opcode == 4'b0000)
        dp33 = opa[31] & opb[31];
      else if (opcode == 4'b0001)
        dp33 = opa[31] ^ opb[31];
      else if (opcode == 4'b0010)
        dp33 = ((opa>>1) - (opb>>1) - ((!opa[0])&opb[0]))>>31;
      else if (opcode == 4'b0011)
        dp33 = ((opb>>1) - (opa>>1) - ((!opb[0])&opa[0]))>>31;
      else if (opcode == 4'b0100)
        dp33 = ((opa>>1) + (opb>>1) + (opa[0]&opb[0]))>>31;
      else if (opcode == 4'b0101)
        dp33 = ((opa>>1) + (opb>>1) + ((opa[0]&opb[0])|(`PSR_C&opa[0])|(`PSR_C&opb[0])))>>31;
      else if (opcode == 4'b0110)
        dp33 = ((opa>>1) - (opb>>1) - ((!opa[0])&opb[0]&(!`PSR_C)))>>31;
      else if (opcode == 4'b0111)
        dp33 = ((opb>>1) - (opa>>1) - ((!opb[0])&opa[0]&(!`PSR_C)))>>31;
      else if (opcode == 4'b1000)
        dp33 = opa[31] & opb[31];
      else if (opcode == 4'b1001)
        dp33 = opa[31] ^ opb[31];
      else if (opcode == 4'b1010)
        dp33 = ((opa>>1) - (opb>>1) - ((!opa[0])&opb[0]))>>31;
      else if (opcode == 4'b1011)
        dp33 = ((opa>>1) + (opb>>1) + (opa[0]&opb[0]))>>31;
      else if (opcode == 4'b1100)
        dp33 = opa[31] | opb[31];
      else if (opcode == 4'b1101)
        dp33 = opb[31];
      else if (opcode == 4'b1110)
        dp33 = opa[31] & (~opb[31]);
      else
        dp33 = ~opb[31];
    end
  endfunction

  function integer dp32;
    input [31:0] opcode,opa,opb,psr;
    begin    
      if      (opcode == 4'b0000)
        dp32 = opa & opb;
      else if (opcode == 4'b0001)
        dp32 = opa ^ opb;
      else if (opcode == 4'b0010)
        dp32 = opa - opb;
      else if (opcode == 4'b0011)
        dp32 = opb - opa;
      else if (opcode == 4'b0100)
        dp32 = opa + opb;
      else if (opcode == 4'b0101)
        dp32 = opa + opb + `PSR_C;
      else if (opcode == 4'b0110)
        dp32 = opa - opb + `PSR_C;
      else if (opcode == 4'b0111)
        dp32 = opb - opa + `PSR_C;
      else if (opcode == 4'b1000)
        dp32 = opa & opb;
      else if (opcode == 4'b1001)
        dp32 = opa ^ opb;
      else if (opcode == 4'b1010)
        dp32 = opa - opb;
      else if (opcode == 4'b1011)
        dp32 = opa + opb;
      else if (opcode == 4'b1100)
        dp32 = opa | opb;
      else if (opcode == 4'b1101)
        dp32 = opb;
      else if (opcode == 4'b1110)
        dp32 = opa & (~opb);
      else
        dp32 = ~opb;
      $display("dp32 result: %h", dp32);
    end
  endfunction


//9/12/99 MarkW's bug fix for CC 

  function integer f;    
    input [31:0] dpres32,dpres,ir,opa,opb,v;
    begin
      if ((ir[24:21] == 4'b0010)|(ir[24:21] == 4'b0110)|(ir[24:21] == 4'b1010)| //SUB,SBC,CMP
          (ir[24:21] == 4'b0011)|(ir[24:21] == 4'b0111))                     //RSB,RSC
        f = (dpres[31]<<3)|((dpres==0)<<2)|(dpres32<<1)|
             (((opa[31]!=opb[31])?(dpres32!=dpres[31]):1'b0));
      else if ((ir[24:21] == 4'b0100)|(ir[24:21] == 4'b0101)|(ir[24:21] == 4'b1011)) //ADD,ADC,CMN
        f = (dpres[31]<<3)|((dpres==0)<<2)|(dpres32<<1)|
             (((opa[31]==opb[31])?(dpres32!=dpres[31]):1'b0));
      else
        f = (dpres[31]<<3)|((dpres==0)<<2)|(dpres32<<1)| v;  //7/22/99
    end
  endfunction

function integer condcode;
  input [31:0] ir2, opa, opb, psr;

  begin
    condcode = f(dp33(ir2[24:21],opa,opb,psr),
                 dp32(ir2[24:21],opa,opb,psr),
                 ir2,opa,opb,(`PSR_V))<<28;
  end
endfunction

  function integer signext;
    input [31:0] u,low;
    begin
      if (u) 
        signext = low;
      else
        signext = -low;
    end
  endfunction

  initial $readmemh("prog.hex", m, 0, 127) ;

  always
   begin
     $display("starting");
     @(posedge sysclk) `ENS;
      `PC <= @(posedge sysclk) 0;
      halt <= @(posedge sysclk) 1;
      psr <= @(posedge sysclk) 0;
      l <= @(posedge sysclk) 0;
      b <= @(posedge sysclk) 0;
      supervisor <= @(posedge sysclk) 1;
      
      forever
        begin
          $display("next cycle");
          @(posedge sysclk) `ENS;
            if (halt)
             begin
              while (~cont)
                $display("decoding instruction %h (1)", ir2);
                begin
                  @(posedge sysclk) `ENS; //(`IDLE);
                  halt <= @(posedge sysclk) 0;
                
                  ir1 <= @(posedge sysclk) r[4'h1];
                  ir2 <= @(posedge sysclk) r[4'h2];
                end
               @(posedge sysclk) `ENS;
                  ir1 <= @(posedge sysclk) 32'hf0000000;
                  ir2 <= @(posedge sysclk) 32'hf0000000;
                  `PC <= @(posedge sysclk) 0;  //7/10/99
                  psr <= @(posedge sysclk) 0;
                  l <= @(posedge sysclk) 1;  
                  b <= @(posedge sysclk) 0;
                  supervisor <= @(posedge sysclk) 1;
             end
            else if (intreq&(~supervisor))   //7/1/99
             begin
             $display("decoding instruction %h (2) mode = %b", ir2, `PSR_T);
                ir1 <= @(posedge sysclk) 32'hf0000000;
                ir2 <= @(posedge sysclk) 32'hf00f0000;  //dest R15, use t+4 next cycle 6/25/99
                l <= @(posedge sysclk) 0;   //write memory next cycle
                t <= @(posedge sysclk) `PC;
                `PC <= @(posedge sysclk) `VECT_EXTINT;
                supervisor <= @(posedge sysclk) 1;                          
              @(posedge sysclk) `ENS;   
                $display("entering supervisor mode external interrupt");
                l <= @(posedge sysclk) 1;
                `PC <= @(posedge sysclk) `PC + 4;
             end
            else
             $display("decoding instruction ir1 %h (3) mode = %b", ir1, `PSR_T);
             $display("decoding instruction ir2 %h (3) mode = %b", ir2, `PSR_T);
             begin
                if (condx(ir2[31:28],psr) && // BX
                     (ir2[27:4] == 24'h12fff1))
                  begin
                    $display("BX R%d", ir2[3:0]);
                    ir1 <= @(posedge sysclk) 32'hf0000000;
                    ir2 <= @(posedge sysclk) 32'hf0000000;
                  end
                else if (condx(ir2[31:28],psr) &&
                     ((ir2[27:25] == 3'b101)
                      || (ir2[27:26] == 2'b00 && ir2[15:12] == 4'b1111)))
                  begin
                    $display("B/BL %h", ir2[23:0]);
                    ir1 <= @(posedge sysclk) 32'hf0000000;
                    ir2 <= @(posedge sysclk) 32'hf0000000;
                  end
                else if ((ir1[27:22]==0)&&(ir1[7:4]==9))  // MUL/MLA 5/13/99
                  begin 
                    ir2 <= @(posedge sysclk) {12'hf00,ir1[11:8],12'h000,ir1[3:0]};  
                        //NOP with `OPA and `OPB for next cycle
                  end
                else
                  begin
                    `PC <= @(posedge sysclk) `PC + 4;
                    pc2 <= @(posedge sysclk) pc;         // 5/10/99
                    ir1 <= @(posedge sysclk) m[`PC>>2];
                    ir2 <= @(posedge sysclk) ir1;
                  end
               
                if (condx(ir2[31:28],psr))
                  begin
                    if      ((ir2[27:24]==4'b0001) /* 7/1 &ir2[7]*/ &ir2[4] &(~supervisor))    //6/28/99
                      begin
                        @(posedge sysclk) `ENS;   //unimplemented on ARM1,AWE
                          ir1 <= @(posedge sysclk) 32'hf0000000;
                          ir2 <= @(posedge sysclk) 32'hf00f0000;  //dest R15, use t+4 next cycle 6/25/99
                          l <= @(posedge sysclk) 0;   //write memory next cycle
                          t <= @(posedge sysclk) `PC;
                          `PC <= @(posedge sysclk) `VECT_UNIMP1;
                          supervisor <= @(posedge sysclk) 1;                          
                        @(posedge sysclk) `ENS;   
                          $display("entering supervisor mode x1xxxx9x,dp var shf/rot");
                          l <= @(posedge sysclk) 1;
                          `PC <= @(posedge sysclk) `PC + 4;
                      end
                    else if (ir2[27:4] == 24'h12fff1) // BX
                      begin
                        $display("executing BX");
                        `PC <= @(posedge sysclk) r[ir2[3:0]];
                        `PSR_T <= @(posedge sysclk) !`PSR_T;
                      end
                    else if  (ir2[27:26] == 2'b00)
                      begin
                        if (ir2[24:23] != 2'b10)    // 5/19/99
                          `RD <= `NCLK dp32(ir2[24:21],`OPA,`OPB,psr);
                        if (ir2[20])
                          psr <= @(posedge sysclk) condcode(ir2,`OPA,`OPB,psr);
                        if (ir2[15:12] == 4'b1111)      //5/8/99 shadow pc
                          begin
                            @(posedge sysclk) `ENS;
                              `PC <= r[4'hf]; 
                          end
                      end
                    else if (ir2[27:25] == 3'b101)
                      begin
                        $display("executing B/BL");
                        `PC <= @(posedge sysclk) `PC + `OFFSET4;
                        if (ir2[24])                  //5/10/99
                          `RD <= @(posedge sysclk) pc2;                      
                        if (ir2[23:0] == 24'hfffffe)  //6/27/99
                         begin
                          halt <= @(posedge sysclk) 1;
                         end
                  end                                           
                else if (~supervisor)//6/24/99
                      begin
                        @(posedge sysclk) `ENS;   //ARM inst unimplemented on AWE 6/24/99
                          ir1 <= @(posedge sysclk) 32'hf0000000;
                          ir2 <= @(posedge sysclk) 32'hf00f0000;  //dest R15, use t+4 next cycle 6/25/99
                          l <= @(posedge sysclk) 0;   //write memory next cycle
                          t <= @(posedge sysclk) `PC;
                          `PC <= @(posedge sysclk) `VECT_UNIMP1;
                          supervisor <= @(posedge sysclk) 1;                          
                        @(posedge sysclk) `ENS;   
                          $display("entering supervisor mode");
                          l <= @(posedge sysclk) 1;
                          `PC <= @(posedge sysclk) `PC + 4;
                      end
                  end                                           
                if ((ir1[27:22]==0)&&(ir1[7:4]==9))  // MUL/MLA 5/13/99
                  begin 
                      dest <= @(posedge sysclk) ir1[19:16];  // MUL/MLA 5/13/99
                    @(posedge sysclk) `ENS;
                      if (condx(ir1[31:28],psr))
                        begin
                          `ifdef FAST_MUL                        
                            `RD <= `NCLK `OPA*`OPB; //7/22/99 1 cyc parallel *
                          `endif
                          `ifdef SERIAL_MUL
                              t <= @(posedge sysclk) `OPB;  //7/22/99 33 cyc serial *
                                ir2 <= @(posedge sysclk)  
                                  //{12'he3a,4'h0,dest,12'h000}; 
                                  {{12'he3a,4'h0},dest[3:0]}<<12; 
                                     //MOV dest,0                         
                              cnt <= @(posedge sysclk) 31;
                             @(posedge sysclk) `ENS;
                              `RD <= `NCLK dp32(ir2[24:21],`OPA,`OPB,psr);
                              while (cnt != 0)
                                begin
                                  if (t[31])
                                     ir2 <= @(posedge sysclk)  
                                       ({{12'he08,ir1[11:8]},dest[3:0]}<<12)|8'h80|dest;
                                        //ADD dest,`OPA,dest LSL 1
                                  else
                                     ir2 <= @(posedge sysclk)  
                                       ({{12'he1a,4'h0},dest[3:0]}<<12)|8'h80|dest; 
                                        //MOV dest,dest LSL 1
                                  t <= @(posedge sysclk) t << 1;  
                                 @(posedge sysclk) `ENS;
                                  `RD <= `NCLK dp32(ir2[24:21],`OPA,`OPB,psr);
                                  cnt <= @(posedge sysclk) cnt - 1;
                                end
                          `endif                                
                          if (ir1[21])
                            ir2 <= @(posedge sysclk)  //MLA
                             ({{12'he08|ir1[20],dest[3:0]},dest[3:0]}<<12)|ir1[15:12];
                                     //ADD[S] dest,dest,ir1[15:12]
                          else
                            ir2 <= @(posedge sysclk)  //MUL
                             ({{12'he1a|ir1[20],4'h0},dest[3:0]}<<12)|dest; 
                                    //MOV[S] dest,dest
                        end
                      else
                         ir2 <= @(posedge sysclk) 32'hf0000000;
                      `PC <= @(posedge sysclk) `PC + 4;
                      pc2 <= @(posedge sysclk) pc;         // 5/10/99
                      ir1 <= @(posedge sysclk) m[`PC>>2];
                  end
                else if ((ir1[27:25]==3'b011) & (ir1[11:4] != 0) & (~supervisor)) 
                      begin
                        @(posedge sysclk) `ENS;   //rvar shf/rot unimplemented on AWE
                          ir1 <= @(posedge sysclk) 32'hf0000000;
                          ir2 <= @(posedge sysclk) 32'hf00f0000;  //dest R15, use t+4 next cycle 6/25/99
                          l <= @(posedge sysclk) 0;   //write memory next cycle
                          t <= @(posedge sysclk) `PC+4;
                          `PC <= @(posedge sysclk) `VECT_UNIMP1;
                          supervisor <= @(posedge sysclk) 1;                          
                        @(posedge sysclk) `ENS;   
                          $display("entering supervisor mode LDM/STM shf/rot");
                          l <= @(posedge sysclk) 1;
                          `PC <= @(posedge sysclk) `PC + 4;
                      end
                else if (ir1[27:26]==2'b01)              //5/16/99 STR/LDR
                  begin
                      dest <= @(posedge sysclk) ir1[19:16];  // `W |~`P 
                    @(posedge sysclk) `ENS; //new_state(`LDASTA1);
                      dest <= @(posedge sysclk) ir2[15:12];  //LDASTA2
                      t <= @(posedge sysclk) `PC;// + 4;
                      ir2 <=  @(posedge sysclk) {12'hf00,ir2[15:12],16'h0000};  
                                    // NOP with`OPA for next cycle
                      if (condx(ir2[31:28],psr))
                        begin
                            l <= @(posedge sysclk) `L;
                            b <= @(posedge sysclk) `B;
                            if (`P)
                              `PC <= @(posedge sysclk) `OPA + (`U?ir2[11:0]:(0-ir2[11:0]));                          
                              //`PC <= @(posedge sysclk) `OPA + signext(`U,ir2[11:0]);                          
                            else
                              `PC <= @(posedge sysclk) `OPA;                        
                            if (`W|(~`P))
                              `RD <= `NCLK `OPA + (`U?ir2[11:0]:(0-ir2[11:0]));                           
                              //`RD <= `NCLK `OPA + signext(`U,ir2[11:0]);                           

                         @(posedge sysclk) `ENS; //(`LDASTA2);
                            l <= @(posedge sysclk) 0;
                            b <= @(posedge sysclk) 0;
                            if (l) 
                              begin
                                `RD <= `NCLK m[`PC >> 2];
                                if (ir2[19:16]==15) /*OPA bits indicate pc*/
                                  begin
                                    @(posedge sysclk) `ENS; //(`LDAPC1); //5/18/99
                                      ir1 <= @(posedge sysclk) 32'hf0000000;
                                      ir2 <= @(posedge sysclk) 32'hf0000000;
                                      t <= @(posedge sysclk) r[4'hf];
                                    @(posedge sysclk) `ENS; //(`LDAPC2);
                                  end
                              end 
                            else
                              begin
                                if (ir2[19:16]==15) /*OPA bits indicate pc*/
                                  m[`PC >> 2] <=  t+4;
                                else
                                  m[`PC >> 2] <= `OPA;
                              end
                        end
                      else
                        begin
                          @(posedge sysclk) `ENS; //(`LDASTA3);
                        end
                      `PC <= @(posedge sysclk) t;
                  end     
                else if (ir1[27:25] == 3'b101)
                  dest <= @(posedge sysclk) 14;          //BL 5/10/99
                else
                  dest <= @(posedge sysclk) ir1[15:12]; 
              end
        end
    end    
endmodule

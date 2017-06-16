// module  arm7_system(cont,sysclk,x,y,ir1,ir2);

module counter_tb; 
  reg clk; 
  wire[31:0] ir1, ir2;
    
  arm7_system a0 ( 
  .cont    (1'b1), 
  .sysclk  (clk), 
  .ir1     (ir1), 
  .ir2     (ir2) 
  ); 
    
  initial 
  begin 
    clk = 0; 
  end 
    
  always 
    #5 clk = !clk; 

//  always
//    begin
//    #5 $display("Executing IR1=%h IR2=%h", ir1, ir2);
//    end
    
endmodule 

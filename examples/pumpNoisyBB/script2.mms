add_input_variables SignCritMeth SignNotCritMeth SignHighWater SignLowWater TurnPumpOn TurnPumpOff
add_state_variables CritMeth HighWater PumpOn
set_scalar_inputs 0 1 2 3 4 5
use_scalar_inputs

set_state_x 0 200 
set_state_x 1 300
set_state_x 2 200
set_state_x 3 300
set_state_x 4 100
set_state_x 5 200
set_state_x 6 100
set_state_x 7 200
set_state_y 0 100
set_state_y 1 200
set_state_y 2 200
set_state_y 3 300
set_state_y 4 200
set_state_y 5 300
set_state_y 6 300
set_state_y 7 400


load_network C:\Passarel\TempExp\ExpA1.xml 1
network_eta 0.1
use_file_none
load_sequences 1
3
-1 -1 -1
1 0 0 0 0 0 
0 0 1 0 0 0
0 0 0 0 1 0
0 0 -1
not_reinforce_current_output
not_allow_learning
execute 1 10000 0 10000
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp2_before.wmf
allow_learning
execute 30 10000 0 10000
save_network C:\Passarel\TempExp\ExpB1N0\iExp2.xml
save_history C:\Passarel\TempExp\ExpB1N0\iExp2Hist.txt 
save_logic_program_xtra C:\Passarel\TempExp\ExpB1N0\iExp2.plg
save_RMSE_history C:\Passarel\TempExp\ExpB1N0\iExp2.txt
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp2.wmf
save_RMSE_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp2_RMSE.wmf

reset_training

load_network C:\Passarel\TempExp\ExpA1.xml 2
network_eta 0.1
use_file_none
load_sequences 1
3
-1 -1 -1
1 0 0 0 0 0 
0 0 1 0 0 0
0 0 0 0 1 0
0 0 -1
not_reinforce_current_output
not_allow_learning
execute 1 10000 0 10000
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp3_before.wmf
allow_learning
execute 30 10000 0 10000
save_network C:\Passarel\TempExp\ExpB1N0\iExp3.xml
save_history C:\Passarel\TempExp\ExpB1N0\iExp3Hist.txt 
save_logic_program_xtra C:\Passarel\TempExp\ExpB1N0\iExp3.plg
save_RMSE_history C:\Passarel\TempExp\ExpB1N0\iExp3.txt
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp3.wmf
save_RMSE_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp3_RMSE.wmf

reset_training

load_network C:\Passarel\TempExp\ExpA1.xml 3
network_eta 0.1
use_file_none
load_sequences 1
3
-1 -1 -1
1 0 0 0 0 0 
0 0 1 0 0 0
0 0 0 0 1 0
0 0 -1
not_reinforce_current_output
not_allow_learning
execute 1 10000 0 10000
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp4_before.wmf
allow_learning
execute 30 10000 0 10000
save_network C:\Passarel\TempExp\ExpB1N0\iExp4.xml
save_history C:\Passarel\TempExp\ExpB1N0\iExp4Hist.txt 
save_logic_program_xtra C:\Passarel\TempExp\ExpB1N0\iExp4.plg
save_RMSE_history C:\Passarel\TempExp\ExpB1N0\iExp4.txt
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp4.wmf
save_RMSE_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp4_RMSE.wmf

reset_training

load_network C:\Passarel\TempExp\ExpA1.xml 4
network_eta 0.1
use_file_none
load_sequences 1
3
-1 -1 -1
1 0 0 0 0 0 
0 0 1 0 0 0
0 0 0 0 1 0
0 0 -1
not_reinforce_current_output
not_allow_learning
execute 1 10000 0 10000
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp5_before.wmf
allow_learning
execute 30 10000 0 10000
save_network C:\Passarel\TempExp\ExpB1N0\iExp5.xml
save_history C:\Passarel\TempExp\ExpB1N0\iExp5Hist.txt 
save_logic_program_xtra C:\Passarel\TempExp\ExpB1N0\iExp5.plg
save_RMSE_history C:\Passarel\TempExp\ExpB1N0\iExp5.txt
save_diagram_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp5.wmf
save_RMSE_as_WMF C:\Passarel\TempExp\ExpB1N0\iExp5_RMSE.wmf

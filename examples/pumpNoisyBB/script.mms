add_input_variables SignCritMeth SignNotCritMeth SignHighWater SignLowWater TurnPumpOn TurnPumpOff
add_state_variables CritMeth HighWater PumpOn

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

load_network FF_9_6_3.xml
network_eta 0.05
load_datafile 35percent.pid
use_file_all
define_file_groups 1
not_reinforce_current_output
not_allow_learning
execute 1 1000 0 1000
save_diagram_as_WMF 35percent_before.wmf
allow_learning
execute 500 10000 0 10000
save_network 35percent.xml
save_history 35percent_Hist.txt 
save_logic_program_xtra 35percent.plg
save_RMSE_history 35percent.txt
save_diagram_as_WMF 35percent.wmf
save_RMSE_as_WMF 35percent_RMSE.wmf

reset_training

load_network FF_9_6_3.xml
network_eta 0.05
load_datafile 40percent.pid
use_file_all
define_file_groups 1
not_reinforce_current_output
not_allow_learning
execute 1 1000 0 1000
save_diagram_as_WMF 40percent_before.wmf
allow_learning
execute 500 10000 0 10000
save_network 40percent.xml
save_history 40percent_Hist.txt 
save_logic_program_xtra 40percent.plg
save_RMSE_history 40percent.txt
save_diagram_as_WMF 40percent.wmf
save_RMSE_as_WMF 40percent_RMSE.wmf

reset_training

load_network FF_9_6_3.xml
network_eta 0.05
load_datafile 45percent.pid
use_file_all
define_file_groups 1
not_reinforce_current_output
not_allow_learning
execute 1 1000 0 1000
save_diagram_as_WMF 45percent_before.wmf
allow_learning
execute 500 10000 0 10000
save_network 45percent.xml
save_history 45percent_Hist.txt 
save_logic_program_xtra 45percent.plg
save_RMSE_history 45percent.txt
save_diagram_as_WMF 45percent.wmf
save_RMSE_as_WMF 45percent_RMSE.wmf

reset_training

load_network FF_9_6_3.xml
network_eta 0.05
load_datafile 50percent.pid
use_file_all
define_file_groups 1
not_reinforce_current_output
not_allow_learning
execute 1 1000 0 1000
save_diagram_as_WMF 50percent_before.wmf
allow_learning
execute 500 10000 0 10000
save_network 50percent.xml
save_history 50percent_Hist.txt 
save_logic_program_xtra 50percent.plg
save_RMSE_history 50percent.txt
save_diagram_as_WMF 50percent.wmf
save_RMSE_as_WMF 50percent_RMSE.wmf

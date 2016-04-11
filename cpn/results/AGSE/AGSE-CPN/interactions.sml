1`[{node="NVIDIA_JetsonTK1", port="sample_state_publisher", 
	operation={node="BBB_UIP", component="userDisplay", 
		operation="sample_state_subscriber", priority=50, deadline=10000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=0, duration=1640}]}},
{node="NVIDIA_JetsonTK1", port="sample_state_publisher", 
	operation={node="BBB_UIP", component="userInputController", 
		operation="sample_state_subscriber", priority=50, deadline=10000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=0, duration=1640}]}},  

(* PayloadBay State Pub-Sub *)
{node="NVIDIA_JetsonTK1", port="payloadbay_state_publisher", 
	operation={node="BBB_UIP", component="userDisplay", 
		operation="payloadbay_state_subscriber", priority=50, deadline=10000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=0, duration=1640}]}},
{node="NVIDIA_JetsonTK1", port="payloadbay_state_publisher", 
	operation={node="BBB_UIP", component="userInputController", 
		operation="payloadbay_state_subscriber", priority=50, deadline=10000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=0, duration=1640}]}},  

(* ArmState Pub-Sub *)			
{node="NVIDIA_JetsonTK1", port="arm_state_publisher", 
	operation={node="BBB_UIP", component="userInputController", 
		operation="arm_state_subscriber", priority=50, deadline=100, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", unblk=[], exec_time=0, duration=200}]}},    	

(* sample_state_from_image_server - Receives requests from sample_state_from_image_client in armController *)
{node="NVIDIA_JetsonTK1", port="sample_state_from_image_client", 
	operation={node="NVIDIA_JetsonTK1", component="imageProcessor", 
		operation="sample_state_from_image_server", priority=50, deadline=10000, enqueue_time=0, 
    			steps=[{kind="CLIENT", port="capture_image_client", unblk=[], exec_time=0, duration=0},
    					{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="armController", port="sample_state_from_image_client"}], 
    																		exec_time=0, duration=800}]}},  

(* payloadbay_state_from_image_server - Receives requests from payloadbay_state_from_image_client in armController *)
{node="NVIDIA_JetsonTK1", port="payloadbay_state_from_image_client", 
	operation={node="NVIDIA_JetsonTK1", component="imageProcessor", 
		operation="payloadbay_state_from_image_server", priority=50, deadline=10000, enqueue_time=0, 
    			steps=[{kind="CLIENT", port="capture_image_client", unblk=[], exec_time=0, duration=0},
    					{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="armController", port="payloadbay_state_from_image_client"}], 
    																		exec_time=0, duration=2200}]}},   

(* capture_image_server - Receives requests from capture_image_client in imageProcessor *)
{node="NVIDIA_JetsonTK1", port="capture_image_client", 
	operation={node="NVIDIA_JetsonTK1", component="imageSensor", 
		operation="capture_image_server", priority=50, deadline=50000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="imageProcessor", port="capture_image_client"}], 
    																		exec_time=0, duration=1570}]}},    



(* radial_position_server - Receives requests from radial_position_client in armController *)
{node="NVIDIA_JetsonTK1", port="radial_position_client", 
	operation={node="BBB_Servos", component="radialController", 
		operation="radial_position_server", priority=50, deadline=1000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="armController", port="radial_position_client"}], 
    																		exec_time=0, duration=143}]}},

(* vertical_position_server - Receives requests from vertical_position_client in armController *)
{node="NVIDIA_JetsonTK1", port="vertical_position_client", 
	operation={node="BBB_Servos", component="verticalController", 
		operation="vertical_position_server", priority=50, deadline=1000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="armController", port="vertical_position_client"}], 
    																		exec_time=0, duration=99}]}},

(* arm_rotation_server - Receives requests from arm_rotation_client in armController *)
{node="NVIDIA_JetsonTK1", port="arm_rotation_client", 
	operation={node="BBB_Servos", component="rotationalController", 
		operation="arm_rotation_server", priority=50, deadline=1000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="armController", port="arm_rotation_client"}], 
    																		exec_time=0, duration=372}]}},   

(* gripper_rotation_server - Receives requests from gripper_rotation_client in armController *)
{node="NVIDIA_JetsonTK1", port="gripper_rotation_client", 
	operation={node="BBB_Servos", component="rotationalController", 
		operation="gripper_rotation_server", priority=50, deadline=1000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="armController", port="gripper_rotation_client"}], 
    																		exec_time=0, duration=99}]}},   

(* gripper_position_server - Receives requests from gripper_position_client in armController *)
{node="NVIDIA_JetsonTK1", port="gripper_position_client", 
	operation={node="BBB_Servos", component="rotationalController", 
		operation="gripper_position_server", priority=50, deadline=1000, enqueue_time=0, 
    			steps=[{kind="LOCAL", port="LOCAL", 
    			unblk=[{node="NVIDIA_JetsonTK1", component="armController", port="gripper_position_client"}], 
    																		exec_time=0, duration=2}]}}]   

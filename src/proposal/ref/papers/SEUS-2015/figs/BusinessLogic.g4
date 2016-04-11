grammar BusinessLogic;

@parser::members
{
	protected const int EOF = Eof;
}

@lexer::members
{
	protected const int EOF = Eof;
	protected const int HIDDEN = Hidden;
}

/*
 * Parser Rules
 */

/*
 * Start of the grammar - The grammar consists of business logic operations. 
 */
start: (operation)*;

/*
 * Each operation starts with a keyword 'Do' followed by:
 * (1) The Name of the operation - op_name
 * (2) The Operation Priority - op_priority
 * (3) The Operation Deadline - op_deadline
 * (4) Zero of more 'steps' 
 */
operation: 'Do' op_name '[' op_priority ',' op_deadline ']' '{' (step)* '};' ;

/*
 * Name of a Component Operation.
 */
op_name: ID;

/*
 * Priority of a Component Operation.
 */
op_priority: INT;

/*
 * Deadline of a Component Operation.
 */
op_deadline: INT;

/*
 * Each step in a component operation can be one of the following:
 * (1) Fragment of code
 * (2) RMI call
 * (3) AMI call
 * (4) DDS publish step
 * (5) DDS pull subscribe step
 * (6) DDS push subscribe step
 * (7) Control Loop
 */
step: code | rmi_call | ami_call | dds_publish | dds_pull_subscribe | dds_push_subscribe | loop;

/*
 * A code fragment is represented by an integral amount of time (milliseconds) it takes to execute
 * Note: This execution time is assumed to be the worst-case time taken by this fragment of code.
 */
code: INT;

/*
 * An RMI call is represented by the keyword 'RMI' followed by:
 * (1) The name of the receptacle port used
 * (2) The name of the remote operation
 * (3) rmi_q_t - Worst-case time taken by push out an RMI query.
 * (4) rmi_pr_t - Worst-case time taken to process the response obtained from a server.
 * Note: This execution time is assumed to be the worst-case time taken by this fragment of code.
 */
rmi_call: 'RMI' receptacle_port '.' remote_op '[' rmi_q_t ',' rmi_pr_t '];';

/*
 * Name of the receptacle port used for RMI and AMI calls.
 */
receptacle_port: ID;

/*
 * Name of the remote operation queried by RMI and AMI calls.
 */
remote_op: ID;

/*
 * Worst-case time taken to push out an RMI query.
 */
rmi_q_t: INT;

/*
 * Worst-case time taken to process a RMI response from a server.
 */
rmi_pr_t: INT;

/*
 * An AMI call is represented by the keyword 'AMI' followed by:
 * (1) The name of the receptacle port used
 * (2) The name of the remote operation
 * (3) ami_q_t - Worst-case time taken by push out an RMI query.
 * (4) ami_pr_t - Worst-case time taken to process the response obtained from a server.
 * Note: This execution time is assumed to be the worst-case time taken by this fragment of code.
 */
ami_call: 'AMI' receptacle_port '.' remote_op '[' ami_q_t ',' ami_pr_t '];';

/*
 * Worst-case time taken to push out an AMI query.
 */
ami_q_t: INT;

/*
 * Worst-case time taken to process a AMI response from a server
 */
ami_pr_t: INT;

/*
 * An DDS Publish call is represented by the keyword 'DDS_Publish' followed by:
 * (1) The name of the publisher port
 * (2) The name of the DDS topic
 * (3) publish_time - Worst-case time taken to publish on the DDS topic.
 * Note: This execution time is assumed to be the worst-case time taken by this fragment of code.
 */
dds_publish: 'DDS_Publish' dds_publisher_port '.' dds_topic '[' publish_time '];';

/*
 * Name of a DDS Publisher Port
 */
dds_publisher_port: ID;

/*
 * Name of a DDS Topic
 */
dds_topic: ID;

/*
 * Worst-case time taken to publish on the DDS topic
 */
publish_time: INT;

/*
 * An DDS Pull Subscribe call is represented by the keyword 'DDS_Pull_Susbcribe' followed by:
 * (1) The name of the subscriber port
 * (2) The name of the DDS topic
 * (3) listen_time - Worst-case time taken to pull on the DDS topic.
 * Note: This execution time is assumed to be the worst-case time taken by this fragment of code.
 */
dds_pull_subscribe: 'DDS_Pull_Subscribe' dds_pull_port '.' dds_topic '[' listen_time '];';

/*
 * Name of a Pull Subscriber port.
 */
dds_pull_port: ID;

/*
 * Worst-case time taken to process a DDS topic item using pull subscription.
 */
listen_time : INT;

/*
 * An DDS Push Subscribe call is represented by the keyword 'DDS_Push_Subscribe' followed by:
 * (1) The name of the subscriber port
 * (2) The name of the DDS topic
 * (3) process_datum_time - Worst-case time taken to process the datum that is read.
 * Note: This execution time is assumed to be the worst-case time taken by this fragment of code.
 */
dds_push_subscribe: 'DDS_Push_Subscribe' dds_push_port '.' dds_topic '[' process_datum_time ']';

/*
 * Name of a Push Subscriber port.
 */
dds_push_port: ID;

/*
 * Worst-case time taken to process a DDS topic item using push subscription.
 */
process_datum_time: INT;


/*
 * A control loop starts with the keyword 'LOOP' followed by:
 * (1) count - The number of iterations in the loop
 * (2) One or more steps.
 */
loop:	// A loop repeats one or more steps a 'count' number of times 
	    'LOOP' '[' count ']' '{' ( step )+ '}'
	;

/*
 * The number of iterations in a control loop
 */
count: INT;

compileUnit: EOF;


/*
 * Lexer Rules
 */

/*
 * An ID - One or more alphanumeric characters that must start with either an alphabet/underscore.
 */
ID:	('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*;

INT: '0'..'9'+;

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) -> channel(HIDDEN)
    ;

COMMENT
    :   '/*' .*? '*/' -> skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;

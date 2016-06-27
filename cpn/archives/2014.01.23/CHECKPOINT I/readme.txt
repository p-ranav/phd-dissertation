1 Thread in partition 1
Has three ops in queue
The thread is uncontested, so it gets scheduled.

Each op takes 2 ticks to complete. no blocking

While the first op ticks along, the other op's wait_time gets incremented by 1

--> This is important when checking dl violation --> time for which i'm waiting in the op_queue needs to be considered. 
-----> so when checking for dl violation, you must check (op_et - op_st) + op_wt < op_dl


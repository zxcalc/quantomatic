
use "single_assignment.ML";
if Multithreading.available then ()
else use "single_assignment_sequential.ML";

if Multithreading.available
then use "bash.ML"
else use "bash_sequential.ML";

use "par_exn.ML";
use "task_queue.ML";
use "future.ML";

use "lazy.ML";
if Multithreading.available then ()
else use "lazy_sequential.ML";

use "par_list.ML";
if Multithreading.available then ()
else use "par_list_sequential.ML";

use "mailbox.ML";
use "cache.ML";

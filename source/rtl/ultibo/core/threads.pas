{
Ultibo Threads interface unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:


References
==========


Threads
=======

 Locking Primitives
 ------------------

  Spin - A simple spin lock for fast mutually exclusive access to data. Threads "spin" while waiting to acquire the lock and do not yield the CPU unless pre-empted.
         Non recursive (can only be acquired once by the same thread)
         Includes IRQ/FIQ entry and exit routines to Save/Restore the IRQ/FIQ state.
         Suitable for use by Interrupt handlers if non interrupt callers use the IRQ/FIQ entry and exit routines to Lock and Unlock.
         Suitable for use on multiprocessor systems if the lock is allocated from shared memory (Determined during initialization).
         Not recommended for long held operations or holding during I/O operations.
         Access is not serialized, the next thread to try obtaining the lock when it is released will succeed even if that thread was not the first waiting.

         Usage:
         ------

         Create/Destroy

         Spin locks are created using SpinCreate() and destroyed using SpinDestroy(), these functions should not be called from within an Interrupt handler.

         Lock/Unlock (Data accessed only by threads)

         To synchronise data access between threads each thread should call SpinLock() before accessing the data and SpinUnlock() when finished accessing the data.
         These calls do not affect the state of IRQs or FIQs and therefore do not impact interrupt latency.

         Lock/Unlock (Data accessed by threads and interrupt handlers)

         To use a Spin lock to synchronise data access between threads and an interrupt handler each thread should call SpinLockIRQ() or SpinLockFIQ(), depending on
         whether the interrupt handler is servicing IRQ or FIQ requests, before accessing the data and SpinUnlockIRQ() or SpinUnlockFIQ() when finished accessing the data.
         These calls disable interrupts before acquiring the lock (with deadlock protection) and restore interrupts after releasing the lock and therefore should only be
         used to protect very short sections of code accessing shared data to minimise the impact on interrupt latency.

         Interrupt handlers should call SpinLock() before accessing the data and SpinUnlock() when finished accessing the data. In a Uniprocessor system it is technically
         not neccessary for interrupt handlers to call lock/unlock as the use of IRQ/FIQ disable/enable will prevent interrupt handlers from executing while a thread has the
         lock. On a Multiprocessor system however interrupt handlers can execute on one processor while a thread is executing on another (which will not deadlock), to correctly
         synchronise data access both threads and interrupt handlers should call the appropriate lock/unlock before and after access to the data.

         Lock Hierarchy:
         ---------------
         It is safe to acquire one lock using SpinLock() then another lock using SpinLock() and then release them in reverse order.

         It is also safe to acquire one lock using SpinLock() then another lock using SpinLockIRQ(or FIQ)() and then release them in reverse order.

         It is NOT safe to acquire one lock using SpinLockIRQ(or FIQ)() then another lock using SpinLock() and then release them in reverse order.
         This is because the first SpinLockIRQ(or FIQ)() can disable the scheduling and prevent thread preemption. If another thread is already holding
         the second lock then this sequence will deadlock (Except on a multicore system where the other thread is running on a different CPU).

         It is also safe to acquire one lock using SpinLockIRQ(or FIQ)() then another lock using SpinLockIRQ(or FIQ)() and then release them in reverse order.
         In this case you must ensure that any thread acquiring the second lock also calls SpinLockIRQ(or FIQ)() to thereby avoid the deadlock.

         It is NOT safe to acquire one lock using SpinLockIRQ(or FIQ)() then another lock using SpinLockIRQ(or FIQ)() and then release them in the SAME order.
         If the situation absolutely requires this behaviour then you must use SpinExchangeIRQ(or FIQ)() when holding both locks in order to reverse the order of the
         IRQ or FIQ re-enabling.


  Mutex - A mutually exclusive lock for controlling access to data. Threads yield while waiting to acquire the lock.
          Non recursive (can only be acquired once by the same thread) (Recursive if MUTEX_FLAG_RECURSIVE specified)
          Not suitable for use by Interrupt handlers.
          Suitable for use on multiprocessor systems if the lock is allocated from shared memory (Determined during initialization).
          Recommended for long held operations or holding during I/O operations.
          Access is not serialized, the next thread to try obtaining the lock when it is released will succeed even if that thread was not the first waiting.

          Usage:
          ------

          Create/Destroy

          Mutex locks are created using MutexCreate() and destroyed using MutexDestroy(). The MutexCreateEx() function allows additional options to be specified
          including if the creating thread is the initial owner and flags such as MUTEX_FLAG_RECURSIVE.

          Lock/Unlock

          To use a Mutex lock to synchronise data access between threads each thread should call MutexLock() before accessing the data and MutexUnlock() when
          finished accessing the data.

          The MutexTryLock() function will attempt to lock the Mutex but will return an error instead of waiting for it if already locked.

          Lock Hierarchy:
          ---------------
          It is safe to acquire one lock using MutexLock() then another lock using MutexLock() and then release them in reverse order.

          It is also safe to acquire one lock using MutexLock() then another lock using MutexLock() and then release them in the SAME order.

          It is safe to acquire a Mutex lock using  MutexLock() then aquire a Spin lock using SpinLockIRQ(or FIQ)().

          It is NOT safe to acquire a Spin lock using SpinLockIRQ(or FIQ)() then acquire a Mutex lock using MutexLock().


  CriticalSection - A mutually exclusive lock for serializing access to data. Threads are placed on a wait list while waiting to acquire the lock.
                    Recursive (can be acquired multiple times by the same thread)
                    Not suitable for use by Interrupt handlers.
                    Suitable for use on multiprocessor systems.
                    Recommended for long held operations or holding during I/O operations.
                    Access is serialized, the next thread to obtain the lock when it is released will be the thread that has been waiting longest (See also stolen wakeups below).

                    Usage:
                    ------

                    Create/Destroy

                    Critical Sections are created using CriticalSectionCreate() and destroyed using CriticalSectionDestroy().
                    The CriticalSectionCreateEx() function allows specifying additional flags and options for the lock.

                    Lock/Unlock

                    To use a Critical Section to synchronise data access between threads each thread should call CriticalSectionLock() before
                    accessing the data and CriticalSectionUnlock() when finished accessing the data. A CriticalSectionTryLock() function is also
                    available which will not wait if the lock is unavailable and will return immediately with an error.

                    Because Critical Sections place waiting threads on a wait list they also include the option to specify a timeout
                    value so a thread will not wait indefinitely to obtain the lock by calling CriticalSectionLockEx().

                    The CriticalSectionLockEx() function will return ERROR_SUCCESS if the lock was obtained, ERROR_WAIT_TIMEOUT if the timeout
                    value elapsed before obtaining the lock and ERROR_WAIT_ABANDONED if waiting was abandoned because the lock was destroyed or
                    another thread called ThreadAbandon() with the handle of the waiting thread.

                    It is critical that the return value be checked when using timeouts in order to be certain that the lock was obtained
                    before accessing the protected data, failure to do so may result in data corruption if multiple threads make changes
                    at the same time.

                    Lock Hierarchy:
                    ---------------
                    All serialized locks and synchronization objects generally follow the same principles as Mutex locks above.

                    It is safe to acquire and release most combinations of serialized objects in any order except where doing so
                    would create a deadlock between threads where thread 1 holds object A is waiting on object B and thread 2 holds
                    object B and is waiting on object A.

                    In all cases it is NOT safe to attempt to acquire a serialized object while IRQ or FIQ are disabled such as
                    after calling SpinLockIRQ(or FIQ)()


  Semaphore - A counted serialization object that allows threads to proceed until the count reaches zero after which threads will
              be made to wait until the count is increased by another thread (or interrupt handler) calling signal.
              Suitable for use by Interrupt handlers for signaling only if created with SEMAPHORE_FLAG_IRQ or FIQ (Interrupt handlers must not call wait).
              Suitable for use on multiprocessor systems.
              Access is serialized, the next thread to acquire the semaphore when it is signaled will be the thread that has been waiting longest (See also stolen wakeups below).

              Usage:
              ------

              Create/Destroy

              Semaphores are created using SemaphoreCreate() and destroyed using SemaphoreDestroy(). With additional flags and options
              available by calling SemaphoreCreateEx(). The initial count of the Semaphore must be specified when calling create, in
              many usages the count will begin at zero but any value required by the application can be specified.

              Wait/Signal

              Threads consume the available count (or wait for count to be available) by calling SemaphoreWait() or SemaphoreWaitEx() if
              a timeout value is required.

              Threads (or interrupt handlers) signal available count by calling SemaphoreSignal() or SemaphoreSignalEx() which allows
              increasing the count by values greater than one.

              Lock Hierarchy:
              ---------------
              Similar rules apply to all other serialized objects except that Semaphores support passing the flag SEMAPHORE_FLAG_IRQ (or FIQ)
              to SemaphoreCreateEx() so that signalling via the SemaphoreSignal() functions may be called from an interrupt handler.


  Synchronizer - A reader/writer lock for serializing multiple concurrent reads and single writes to data. Threads are placed on a wait list while waiting to acquire the lock.
                 Recursive (reader lock can be acquired multiple times by the same thread or by other threads / writer lock can be acquired multiple times by the same thread)
                 Not suitable for use by Interrupt handlers.
                 Suitable for use on multiprocessor systems.
                 Recommended for long held operations or holding during I/O operations.
                 Access is serialized, the next thread to obtain the lock when it is released will be the thread that has been waiting longest (See also stolen wakeups below).

                 Usage:
                 ------

                 Create/Destroy

                 Synchronizers are created using SynchronizerCreate() or SynchronizerCreateEx() and destroyed using SynchronizerDestroy().

                 Lock/Unlock

                 Threads wanting read access to the protected data call SynchronizerReaderLock() and SynchronizerReaderUnlock() to obtain and release
                 the lock. While one or more threads holds the reader lock no thread can obtain the writer lock.

                 A thread holding the reader lock can convert it to the writer lock by calling SynchronizerReaderConvert() but if any other thread is
                 also holding the reader lock it will be placed in a wait list to wait until all threads have released the reader lock.

                 Threads wanting write access to the protected data call SynchronizerWriterLock() and SynchronizerWriterUnlock() to obtain and release
                 the lock. While any thread holds the writer lock no thread can obtain the reader lock.

                 A thread holding the writer lock can convert it to the reader lock by calling SynchronizerWriterConvert(), because the writer lock is
                 exclusive the thread will always obtain the reader lock immediately. All other threads currently waiting for the reader lock will also
                 be released and given access.

                 Extended versions of the lock and convert functions for both reader and writer locks are available which allow specifying a timeout
                 value to give up waiting after a specified time.

                 Lock Hierarchy:
                 ---------------
                 The rules for Synchronizers are the same as those that apply to other serialized objects such as CriticalSection.


  Condition - A condition variable is used for coordinating synchronized access to resources by multiple threads. Condition variables are similar in concept to
              both Semaphores and Events but do not retain any internal state information so threads always wait until they are woken or the specified timeout
              interval expires.
              Not suitable for use by Interrupt handlers.
              Suitable for use on multiprocessor systems.
              Access is serialized, the next thread released when a condition is woken will be the thread that has been waiting longest (See also stolen wakeups below).

              Usage:
              ------

              Create/Destroy

              Conditions are created using ConditionCreate() and destroyed using ConditionDestroy().

              Wait/Wake

              Threads call ConditionWait() to begin waiting for the condition to be woken, additional versions of wait exist that allow releasing
              another lock before waiting and reacquiring it when the thread is woken after waiting.

              These are ConditionWaitMutex(), ConditionWaitSynchronizer(), ConditionWaitCriticalSection() which apply to Mutex, Synchronizer and
              CriticalSection objects respectively.

              Calling ConditionWake() will release one thread that is currently waiting for the condition, the ConditionWakeAll() function will
              release all threads waiting for the condition.

              Lock Hierarchy:
              ---------------
              The rules for Conditions are the same as those that apply to other serialized objects such as CriticalSection and Synchronizers.


  Completion - A completion is similar in concept to both condition variables and events but behaves differently to each of them. Completions are designed to be similar to the
               Linux synchronization object of the same name and provide a light weight mechanism for allowing one or more threads to wait for a signal that they can proceed.
               The completion differs from a condition variable because it maintains an internal state, once the state has been set (complete) threads pass through the completion
               without waiting (until the state is reset). This is similar to an event (see below) but in the case of a completion the state remains set indefinitely or until
               reset is called, a completion also allows explicitly releasing one thread at a time or all threads at once. More generally the event was created to model a
               behaviour that is similar to the same object in Windows and the completion models the Linux object instead.

               Completions can use a counted state rather than a single state if they are created with COMPLETION_FLAG_COUNTED, this is to mimic the implementation of the
               Linux variation however there is a slight but important change in the way counted completions are implemented in Ultibo. The Linux completion sets the count
               to UMAX_INT / 2 on complete_all() which is documented as "effectively infinite". This is of course incorrect and seriously flawed because the count value is
               only set to a little over 2 billion, a long running application could easily consume this count with calls to wait_for_completion() and then the application
               would potentially fail without explanation.

               To prevent this same fatal flaw the Ultibo implementation sets the count at LongWord(-1) on CompletionCompleteAll() and all other operations check for this
               value before incrementing or decrementing the count further. In this way the setting of complete all is genuinely infinite and will not fail on a long running
               application.

               Suitable for use by Interrupt handlers to call complete or reset only if created with COMPLETION_FLAG_IRQ or FIQ (Interrupt handlers must not call wait).
               Suitable for use on multiprocessor systems.
               Access is serialized, the next thread released when a completion is set will be the thread that has been waiting longest (See also stolen wakeups below).

               Usage:
               ------

               Create/Destroy

               Completions are created using CompletionCreate() and destroyed using CompletionDestroy().

               Wait/Reset/Complete

               Threads call CompletionWait() to either wait for a completion to be set or proceed immediately if it is already set, for counted completions the
               count is decremented (if not -1) and if the count reaches 0 the state of the completion is reset. A CompletionTryWait() function is available to
               check the state and return immediately with an error if not set.

               Threads (or interrupt handlers) set or reset the state of the completion by calling CompletionComplete(), CompletionReset() or CompletionCompleteAll().
               See the header of each function for a detailed description of the behaviour in all applicable cases.

               Lock Hierarchy:
               ---------------
               The rules for Completions are the same as those that apply to other serialized objects such as CriticalSection and Synchronizers except that
               like Semaphores they may be signalled by interrupt handlers if the appropriate flags as passed to create.


 Thread Handling
 ---------------

  List - A linked list designed to provide the wait list functionality for threads, each thread record includes a list element and the list elements include a
         thread handle to allow resolution in both directions. Lists are used by all serialized objects to hold threads in an ordered wait list that can be
         safely manipulated in all scheduling states.
         Lists are primarily intended to be used internally but are available for use by applications for appropriate cases.
         Suitable for use by Interrupt handlers if created with appropriate flags.
         Suitable for use on multiprocessor systems.

         Usage:
         ------

         Create/Destroy

         Lists are created using ListCreate() or ListCreateEx() and destroyed using ListDestroy().

         Add/Get/Insert/Remove

         A complete set of functions exist for adding, removing and inserting elements into a list and for finding threads within a list.
         See the header of each of the functions in the appropriate section below for more information.

         Lock/Unlock

         A pair of universal lock and unlock functions exist to acquire and release list locks with the appropriate IRQ (or FIQ) handling for the flags the list
         was created with. See ListLock() and ListUnlock() below for more information.


  Queue - An ordered linked list designed to provide scheduler queues for threads, each thread record includes a queue element and the queue elements include a
          thread handle to allow resolution in both directions.
          Queues are primarily intended to be used internally but are available for use by applications for appropriate cases.
          Suitable for use by Interrupt handlers if created with appropriate flags.
          Suitable for use on multiprocessor systems.

          Usage:
          ------

          Create/Destroy

          Queues are created using QueueCreate() or QueueCreateEx() and destroyed using QueueDestroy().

          Enqueue/Dequeue/Insert/Delete/Increment/Decrement

          A range of functions exist for adding, removing and inserting elements into a queue and for managing the elements within a queue.
          See the header of each of the functions in the appropriate section below for more information.

          Lock/Unlock

          A pair of universal lock and unlock functions exist to acquire and release queue locks with the appropriate IRQ (or FIQ) handling for the flags the
          queue was created with. See QueueLock() and QueueUnlock() below for more information.


  Message - A simple record similar to the Windows MSG structure the TMessage contains fields whose purpose is opaque to the core of Ultibo and can be used
            for passing any type of information between threads or through other mechanisms.
            No functions explicity exist to deal with messages, however they are used as the format for thread messages and are the record that is
            passed to ThreadSendMessage() and received from ThreadReceiveMessage(). Messages are also used as the record format for Messageslots (see below).


  Thread - Threads form the primary unit of code execution in Ultibo. It is not possible to create processes which run in their own protected address space,
           all threads run within the kernel and with full privilege so they can perform any operation required. The core of Ultibo always includes
           threading and multiple threads are created during the boot process to perform system critical functions.

           The complete set of available thread functions are listed below and the header of each one includes more information including any
           important notes or restrictions to be aware of.


 Additional Items
 ----------------

  Messageslot - The Messageslot allows multiple threads to wait on a single object for messages received from other sources. Where the thread message functions
                such as ThreadReceiveMessage() only allow a single thread to receive the message many threads can wait simultaneously on a Messageslot and will
                be woken in turn as new messages are received.
                Suitable for use by Interrupt handlers for sending only if created with MESSAGESLOT_FLAG_IRQ or FIQ (Interrupt handlers must not call receive).
                Suitable for use on multiprocessor systems.

                Usage:
                ------

                Create/Destroy

                Messageslots are created using MessageslotCreate() or MessageslotCreateEx() and destroyed using MessageslotDestroy().

                Send/Receive

                Threads wait for messages by calling MessageslotReceive() or MessageslotReceiveEx() to wait with a timeout.

                Messages are sent using MessageslotSend() if the Messageslot is full and no more messages can be queued then an error
                will be returned instead of waiting for more space to be available.


  Mailslot - The Mailslot is similar to the Messageslot above and allows multiple threads to wait for messages to be received. Unlike a Messageslot where the
             sender will not wait if there is no space available to send, in a mailslot both the sender and receiver will wait for either available space
             or received messages. Mailslot messages are also an arbitrary pointer which means practically anything can be sent as long as the content is
             recognized by both the sender and receiver.
             Not suitable for use by Interrupt handlers.
             Suitable for use on multiprocessor systems.

             Usage:
             ------

             Create/Destroy

             Mailslots are created using MailslotCreate() and destroyed using MailslotDestroy().

             Send/Receive

             Threads wait for messages by calling MailslotReceive() or MailslotReceiveEx() to wait with a timeout.

             Messages are sent using MailslotSend() or MailslotSendEx(), if the Mailslot is full and no more messages can be queued the sender will wait
             until more space becomes available.


  Buffer - A circular list which can hold blocks of memory, allocated records or any other type of pointer that needs to be acquired, used and later
           released as part of a data transfer or queuing mechanism. Buffers simply contain pointers so their content is completely up to the user to
           determine, the number of entries and the size of each entry must be specified when creating a Buffer.
           Not suitable for use by Interrupt handlers.
           Suitable for use on multiprocessor systems.

           Usage:
           ------

           Create/Destroy

           Buffers are created using BufferCreate() or BufferCreateEx() and destroyed using BufferDestroy().

           Get/Free

           The next available buffer entry is obtained by calling BufferGet() or BufferGetEx() to specify a timeout.
           When the buffer is no longer required it can be returned to the list by calling BufferFree() and will be made available again.

           During initialization of the Buffer all entries can be enumerated (in order to set initial values etc) by calling BufferIterate()
           however once the Buffer is in operation (and any entries have been obtained) it is not valid to call iterate again.


  Event - Events are a serialization object modelled on the Windows object of the same name, they can be set to allow threads to pass through
          or reset to force threads to wait. Events can also be pulsed to allow a only single waiting thread to pass.
          Not suitable for use by Interrupt handlers.
          Suitable for use on multiprocessor systems.

          Usage:
          ------

          Create/Destroy

          Events are created using EventCreate() or EventCreateEx() and destroyed using EventDestroy().

          Wait/Set/Reset/Pulse

          Threads call EventWait() to either wait for the Event to be set or pass through immediately if already set.

          Calling EventSet(), EventReset() or EventPulse() changes the state of the Event and either allows threads to pass
          or require them to wait depending on the values defined when the Event was created.

          See the description in the header of each function below for detailed information about the behaviour in each state.


  Timer - Timers provide a mechanism to request that a thread from the timer pool perform a specified function call in a certain number
          of milliseconds from the time the request was made. This can be a convenient way to retry an operation after a predetermined
          amount of time or to delay an operation until a certain amount of time has passed. Timers can be created as once off or
          recurring and can pass a supplied pointer to the called function.
          Not suitable for use by Interrupt handlers.
          Suitable for use on multiprocessor systems.

          Usage:
          ------

          Create/Destroy

          Timers are created using TimerCreate() or TimerCreateEx() and destroyed using TimerDestroy().

          Enable/Disable

          Timers can be enabled or disabled using the TimerEnable() and TimerDisable() functions, calling TimerEnableEx() allows
          redefining some of the parameters specified during the call to TimerCreate() so an existing Timer can be reused for
          multiple purposes.


  Worker - The Worker thread pool provides a resource that can be used by system tasks and by applications as a convenient
           and reliable way to allocate work to other threads for asynchronous completion. The number of threads in the Worker
           pool can be dynamically increased or decreased to handle varying workloads. Using Worker threads to offload tasks
           can greatly simplify application design but does require attention be given to thread safety of data that is passed
           to Worker threads.
           Not suitable for use by Interrupt handlers unless the WorkerScheduleIRQ or WorkerScheduleFIQ functions are used.
           Suitable for use on multiprocessor systems.

           Usage:
           ------

           Schedule/Cancel

           A task can be scheduled to a Worker thread by calling WorkerSchedule() or WorkerScheduleEx() depending on the parameters
           that need to be supplied. An already scheduled task can be cancelled by calling WorkerCancel() up until the point where
           the task is assigned to a Worker thread.

           IRQ and FIQ Usage:
           ------------------

           WorkerScheduleIRQ() and WorkerScheduleFIQ() are designed to allow calling from interrupt handlers (IRQ or FIQ) and provide
           deadlock prevention mechanisms.

           WorkerScheduleIRQ() relies on the fact the scheduler will either be bound to an IRQ or an FIQ, in either case IRQ is disabled
           while the scheduler is active and in any calls with queue, list and thread locks that check for the scheduler assignment.
           This means that an IRQ cannot occur when the scheduler is active or while it is blocked by a lock, calls to this function
           from a IRQ interrupt handler cannot deadlock.

           WorkerScheduleFIQ() checks for the assignment of the scheduler, if the scheduler is bound to an FIQ then it proceeds as per
           the IRQ version because the FIQ cannot occur when the scheduler is active or while it is blocked by a lock. If the scheduler
           is bound to an IRQ then this function reverts to using the Tasker instead to transfer FIQ operations to an IRQ handler.

           Both functions only support immediate scheduling of a one time task, delayed or repeating tasks cannot be scheduled.

           It is safe to call either function from non interrupt code in order to allow shared code paths etc, however frequent calls
           to these functions from normal code can affect interrupt latency.


  Tasker - The Tasker is a mechanism for transferring work from a fast interrupt (FIQ) handler to an interrupt (IRQ) handler in order
           to allow access to certain operations which could otherwise deadlock. When the scheduler is bound to an IRQ then all queue,
           list and thread locks disable IRQ in order to prevent preemption by the scheduler, when IRQ is disabled then FIQ is still
           enabled and can preempt code that is holding an IRQ lock. If the FIQ handler attempts to access any of these locks then
           the handler can deadlock waiting for code that it had preempted.

           In normal operation the clock interrupt is used to service the tasker list however it is board specific and could be
           assigned to either a different interrupt or a dedicated interrupt if required.

           Suitable for use by Fast Interrupt handlers and Interrupt handlers (Not required for Interrupt handlers).
           Suitable for use on multiprocessor systems.

           Usage:
           ------

           There are currently five Tasker functions available, TaskerThreadSendMessage(), TaskerMessageslotSend(),
           TaskerSemaphoreSignal(), TaskerCompletionReset() and TaskerCompletionComplete() which each perform the same function
           as their non Tasker equivalent.

           The Tasker list is checked on each clock interrupt for tasks waiting to be triggered, the task is performed
           directly by the clock interrupt so the delay between request and trigger is no more than one clock interrupt
           interval.


 Services
 --------

  In addition to the various synchronization and serialization objects described above the following services are provided
  by this unit:

   ThreadManager
   Thread Creation and Stack allocation
   Thread Termination, Stack deallocation and cleanup
   Thread Functions (Sleep/Kill/Suspend/Resume etc)
   IRQ Thread (Boot process becomes the IRQ or FIQ Thread)
   FIQ Thread
   Idle Thread (An always ready thread which measures utilization)
   Main Thread (Main thread executes PASCALMAIN)
   Timer Threads (Pool of threads to service Timer requests)
   Worker Threads (Pool of threads to service Worker requests)
   Thread Scheduling, Allocation, Priority, Affinity and Migration


 Stolen wakeups
 --------------

  Locking and synchronization types such as CriticalSection, Semaphore, Condition, Event, Messageslot and Mailslot use a
  wait queue internally to determine which thread has been waiting the longest. When a thread is removed from the wait
  queue so it can obtain the lock, semaphore, event etc there is a small window where the lock, semaphore or event may
  be acquired by another thread before the thread that was just woken from the wait queue. This is called a stolen wakeup
  and can happen in many systems including Windows and Linux, in some systems the caller is expected to check the state
  of a predicate value on return in order to detect a stolen wakeup. In Ultibo the code within the lock, semaphore, event
  etc functions automatically checks for a stolen wakeup and takes appropriate action to either return the thread to the
  wait queue or return with a timeout (where appropriate) so that the caller can respond accordingly.

  In no case will a call to a locking or synchronization function fail due to a stolen wakeup, the worst case scenario is
  that a thread may wait longer than it should have (ie it is returned to the back of the queue) or it may return with a
  timeout before the specified timeout value has expired.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Threads;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,HeapManager,Locale,Unicode,SysUtils;

//To Do //Look for:

//Critical


//To Do //All of our Platform and Thread etc functions need to use ThreadSetLastError ?

//To Do //****Deadlock issues****:

                      //All use of SemaphoreWait needs to check for WAIT_ABANDONED on return to allow semaphores to be destroyed
                      //while threads are waiting. Needs to be implemented on ThreadWake/ThreadAbandon -
                      //Same applies to Event, CriticalSection, Mailslot etc etc
                      //Event Mutex/Spin etc should check for result on Lock (No need on Unlock, cannot destroy while locked)

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Thread specific constants}

 {Lock constants}
 LOCK_FLAG_NONE   = $00000000;
 LOCK_FLAG_IRQ    = $00000001;
 LOCK_FLAG_FIQ    = $00000002;
 LOCK_FLAG_IRQFIQ = $00000004;

 {Spin constants}
 SPIN_SIGNATURE = $0FEC3B82;

 {Spin state constants}
 SPIN_STATE_UNLOCKED  = 0;
 SPIN_STATE_LOCKED    = 1;

 {Mutex constants}
 MUTEX_SIGNATURE = $1C5D7FA4;

 {Mutex state constants}
 MUTEX_STATE_UNLOCKED = 0;
 MUTEX_STATE_LOCKED   = 1;

 {Mutex flags constants}
 MUTEX_FLAG_NONE       = $00000000;
 MUTEX_FLAG_RECURSIVE  = $00000001; {Mutex can be locked multiple times by the same thread if set (Must be unlocked the same number of times)}
 MUTEX_FLAG_ERRORCHECK = $00000002; {Mutex will perform a deadlock check if set, will return with an error if already owned by the same thread (and not recursive)}

 {Critical Section constants}
 CRITICAL_SECTION_SIGNATURE = $25F3AE01;

 {Critical Section state constants}
 CRITICAL_SECTION_STATE_UNLOCKED = 0;
 CRITICAL_SECTION_STATE_LOCKED   = 1;

 {Semaphore constants}
 SEMAPHORE_SIGNATURE = $390A1EB4;

 {Semaphore flag constants}
 SEMAPHORE_FLAG_NONE   = LOCK_FLAG_NONE;
 SEMAPHORE_FLAG_IRQ    = LOCK_FLAG_IRQ;
 SEMAPHORE_FLAG_FIQ    = LOCK_FLAG_FIQ;
 SEMAPHORE_FLAG_IRQFIQ = LOCK_FLAG_IRQFIQ;

 {Synchronizer constants}
 SYNCHRONIZER_SIGNATURE = $C5D081FB;

 {Synchronizer state constants}
 SYNCHRONIZER_STATE_UNLOCKED      = 0;
 SYNCHRONIZER_STATE_READER_LOCKED = 1;
 SYNCHRONIZER_STATE_WRITER_LOCKED = 2;

 {Synchronizer flag constants}
 SYNCHRONIZER_FLAG_NONE           = $00000000;
 SYNCHRONIZER_FLAG_READ_PRIORITY  = $00000001;  {Synchronizer prioritises readers over writers} //To Do //Implement these (Change the wait/release priority)
 SYNCHRONIZER_FLAG_WRITE_PRIORITY = $00000002;  {Synchronizer prioritises writers over readers} //To Do //Implement these (Change the wait/release priority)

 {Condition constants}
 CONDITION_SIGNATURE = $D14D3C0A;

 {Condition flag constants}
 CONDITION_FLAG_NONE           = $00000000;

 {Condition lock flag constants}
 CONDITION_LOCK_FLAG_NONE      = $00000000;
 CONDITION_LOCK_FLAG_WRITER    = $00000001; {Condition should release and acquire the writer lock on a Synchronizer when ConditionWaitSynchronizer is called (otherwise release and acquire the reader lock)}

 {Completion constants}
 COMPLETION_SIGNATURE = $FCE24CA1;

 {Completion state constants}
 COMPLETION_STATE_RESET    = 0;
 COMPLETION_STATE_COMPLETE = 1;

 {Completion flag constants}
 COMPLETION_FLAG_NONE    = LOCK_FLAG_NONE;
 COMPLETION_FLAG_IRQ     = LOCK_FLAG_IRQ;    {Disable IRQ during completion operations (Wait/Reset/Complete)}
 COMPLETION_FLAG_FIQ     = LOCK_FLAG_FIQ;    {Disable FIQ during completion operations (Wait/Reset/Complete)}
 COMPLETION_FLAG_IRQFIQ  = LOCK_FLAG_IRQFIQ; {Disable IRQ and FIQ during completion operations (Wait/Reset/Complete)}
 COMPLETION_FLAG_COUNTED = $00000008;        {Use a counted value instead of a single state (Affects behaviour of Wait and Complete)}

 {List constants}
 LIST_SIGNATURE = $4A98BE2A;

 {List type constants}
 LIST_TYPE_NOT_SPECIFIED     = 0;  {A generic thread list without a specific purpose}
 LIST_TYPE_WAIT_SECTION      = 1;  {A Critical Section Wait List}
 LIST_TYPE_WAIT_SEMAPHORE    = 2;  {A Semaphore Wait List}
 LIST_TYPE_WAIT_SYNCHRONIZER = 3;  {A Synchronizer Wait List}
 LIST_TYPE_WAIT_CONDITION    = 4;  {A Condition Wait List}
 LIST_TYPE_WAIT_COMPLETION   = 5;  {A Condition Wait List}
 LIST_TYPE_WAIT_EVENT        = 6;  {An Event Wait List}
 LIST_TYPE_WAIT_THREAD       = 7;  {A Thread Wait List}
 LIST_TYPE_WAIT_MESSAGESLOT  = 8;  {A Messageslot Wait List}
 LIST_TYPE_WAIT_OTHER        = 9;  {Another type of Wait List (Suitable for passing to ThreadWait/ThreadWaitEx/ThreadWaitMultiple/ThreadRelease)}

 {List flag constants}
 LIST_FLAG_NONE   = LOCK_FLAG_NONE;
 LIST_FLAG_IRQ    = LOCK_FLAG_IRQ;
 LIST_FLAG_FIQ    = LOCK_FLAG_FIQ;
 LIST_FLAG_IRQFIQ = LOCK_FLAG_IRQFIQ;

 {Queue constants}
 QUEUE_SIGNATURE = $57A3BF9E;

 {Queue type constants}
 QUEUE_TYPE_NOT_SPECIFIED         = 0;  {A generic thread queue without a specific purpose}
 QUEUE_TYPE_SCHEDULE_SLEEP        = 1;  {A Scheduler Sleep Queue}
 QUEUE_TYPE_SCHEDULE_TIMEOUT      = 2;  {A Scheduler Timeout Queue}
 QUEUE_TYPE_SCHEDULE_TERMINATION  = 3;  {A Scheduler Termination Queue (Threads are placed on termination queue when they exit or are terminated)}
 QUEUE_TYPE_SCHEDULE_NONE         = 4;  {A Scheduler No Priority Queue (Only used for IRQ/FIQ threads which are never selected for scheduling)}
 QUEUE_TYPE_SCHEDULE_IDLE         = 5;  {A Scheduler Idle Priority Queue}
 QUEUE_TYPE_SCHEDULE_LOWEST       = 6;  {A Scheduler Lowest Priority Queue}
 QUEUE_TYPE_SCHEDULE_LOWER        = 7;  {A Scheduler Lower Priority Queue}
 QUEUE_TYPE_SCHEDULE_NORMAL       = 8;  {A Scheduler Normal Priority Queue}
 QUEUE_TYPE_SCHEDULE_HIGHER       = 9;  {A Scheduler Higher Priority Queue}
 QUEUE_TYPE_SCHEDULE_HIGHEST      = 10; {A Scheduler Highest Priority Queue}
 QUEUE_TYPE_SCHEDULE_CRITICAL     = 11; {A Scheduler Critical Priority Queue}

 {Queue flag constants}
 QUEUE_FLAG_NONE        = LOCK_FLAG_NONE;
 QUEUE_FLAG_IRQ         = LOCK_FLAG_IRQ;
 QUEUE_FLAG_FIQ         = LOCK_FLAG_FIQ;
 QUEUE_FLAG_IRQFIQ      = LOCK_FLAG_IRQFIQ;
 QUEUE_FLAG_DESCENDING  = $00000008;
 QUEUE_FLAG_DELTA       = $00000010;

 {Queue key constants}
 QUEUE_KEY_NONE = Integer($7FFFFFFF);       {Null key value returned from an empty Queue}
 QUEUE_KEY_MAX  = Integer($7FFFFFFE);       {Max key that can be ordered in a Queue}
 QUEUE_KEY_MIN  = Integer($80000000);       {Min key that can be ordered in a Queue}

 {Thread constants}
 THREAD_SIGNATURE = $6C2BA10F;

 {Thread type constants}
 THREAD_TYPE_NORMAL = 0;   {A Normal thread (No special case handling)}
 THREAD_TYPE_IDLE   = 1;   {An Idle thread (Used to calculate ultilization and provide an always ready thread)}
 THREAD_TYPE_IRQ    = 2;   {An IRQ thread (Used by the IRQ handler during interrupt time)}
 THREAD_TYPE_FIQ    = 3;   {An FIQ thread (Used by the FIQ handler during interrupt time)}
 THREAD_TYPE_SWI    = 4;   {A Software Interrupt (SWI) thread (Used by the SWI handler during a system call)}

 {Thread flag constants}
 THREAD_FLAG_NONE                = $00000000;
 THREAD_FLAG_PERSIST             = $00000001; {If set thread handle will persist until explicitly destroyed (Otherwise destroyed after termination quantum has elapsed)}
 THREAD_FLAG_CANCELLED           = $00000002; {Indicates that thread has been cancelled, for support of external thread APIs (eg pThreads)(Not used internally by Ultibo)}
 THREAD_FLAG_CANCEL_DISABLE      = $00000004; {Indicates that thread cancellation is disabled for a thread, for support of external thread APIs (eg pThreads)(Not used internally by Ultibo)}
 THREAD_FLAG_CANCEL_ASYNCHRONOUS = $00000008; {Indicates that asynchronous thread cancellation is enabled for a thread, for support of external thread APIs (eg pThreads)(Not used internally by Ultibo)}

 THREAD_FLAG_INTERNAL = THREAD_FLAG_NONE + $80000000; {Note: Temporary value to avoid warning}

 {Thread state constants}
 THREAD_STATE_RUNNING         = 1;          {Thread is currently running}
 THREAD_STATE_READY           = 2;          {Thread is on ready queue}
 THREAD_STATE_SLEEP           = 3;          {Thread is sleeping}
 THREAD_STATE_SUSPENDED       = 4;          {Thread is suspended}
 THREAD_STATE_WAIT            = 5;          {Thread is on a wait list}
 THREAD_STATE_WAIT_TIMEOUT    = 6;          {Thread is on a wait list with timeout}
 THREAD_STATE_RECEIVE         = 7;          {Thread is waiting to receive a message}
 THREAD_STATE_RECEIVE_TIMEOUT = 8;          {Thread is waiting to receive a message with timeout}
 THREAD_STATE_HALTED          = 9;          {Thread has been halted (Due to an unhandled exception etc)}
 THREAD_STATE_TERMINATED      = 10;         {Thread has been terminated}
 //To Do THREAD_STATE_SEND / THREAD_STATE_SEND_TIMEOUT ?  //Who would wake them up ? //Timeout is ok, but what about send only ?

 {Thread priority constants}
 THREAD_PRIORITY_NONE      = 0; {Only used for IRQ/FIQ threads which are never selected for scheduling}
 THREAD_PRIORITY_IDLE      = 1;
 THREAD_PRIORITY_LOWEST    = 2;
 THREAD_PRIORITY_LOWER     = 3;
 THREAD_PRIORITY_NORMAL    = 4;
 THREAD_PRIORITY_HIGHER    = 5;
 THREAD_PRIORITY_HIGHEST   = 6;
 THREAD_PRIORITY_CRITICAL  = 7;

 THREAD_PRIORITY_DEFAULT       = THREAD_PRIORITY_NORMAL;      {Default thread priority}
 THREAD_PRIORITY_MINIMUM       = THREAD_PRIORITY_IDLE;        {Minimum thread priority}
 THREAD_PRIORITY_MAXIMUM       = THREAD_PRIORITY_CRITICAL;    {Maximum thread priority}
 THREAD_PRIORITY_COUNT         = THREAD_PRIORITY_MAXIMUM + 1; {Number of thread priority levels}

 {Additional priority aliases for compatibility}
 THREAD_PRIORITY_BELOW_NORMAL  = THREAD_PRIORITY_LOWER;
 THREAD_PRIORITY_ABOVE_NORMAL  = THREAD_PRIORITY_HIGHER;
 THREAD_PRIORITY_TIME_CRITICAL = THREAD_PRIORITY_CRITICAL;

 {Thread name constants}
 THREAD_NAME_LENGTH = SIZE_64;   {Length of thread name}

 IRQ_THREAD_NAME             = 'IRQ';
 FIQ_THREAD_NAME             = 'FIQ';
 SWI_THREAD_NAME             = 'SWI';
 IDLE_THREAD_NAME            = 'Idle';
 MAIN_THREAD_NAME            = 'Main';
 TIMER_THREAD_NAME           = 'Timer';
 WORKER_THREAD_NAME          = 'Worker';
 TIMER_PRIORITY_THREAD_NAME  = 'Priority Timer';
 WORKER_PRIORITY_THREAD_NAME = 'Priority Worker';
 RTL_THREAD_NAME             = 'RTL Thread';

 {Thread priority constants}
 TIMER_THREAD_PRIORITY           = THREAD_PRIORITY_NORMAL;
 WORKER_THREAD_PRIORITY          = THREAD_PRIORITY_NORMAL;
 TIMER_PRIORITY_THREAD_PRIORITY  = THREAD_PRIORITY_HIGHEST;
 WORKER_PRIORITY_THREAD_PRIORITY = THREAD_PRIORITY_HIGHER;

 {Thread create constants}
 THREAD_CREATE_NONE      = $00000000;
 THREAD_CREATE_SUSPENDED = $00000004;

 {Thread TLS constants}
 THREAD_TLS_FREE    = $00000000;
 THREAD_TLS_USED    = $00000001;
 THREAD_TLS_INVALID = $FFFFFFFF;

 THREAD_TLS_MAXIMUM = SIZE_64;       {The maximum number TLS index slots available}

 {Thread TLS flag constants}
 THREAD_TLS_FLAG_NONE = $00000000;
 THREAD_TLS_FLAG_FREE = $00000001;   {If set then pointer in thread TLS index will be freed on ThreadReleaseTlsIndex or ThreadDestroy}

 {Thread wait constants}
 THREAD_LISTS_MAXIMUM = SIZE_64;     {Maximum number of lists a thread can wait on at the same time}

 {Messageslot constants}
 MESSAGESLOT_SIGNATURE = $B631CE4B;

 {Messageslot flag constants}
 MESSAGESLOT_FLAG_NONE   = LOCK_FLAG_NONE;
 MESSAGESLOT_FLAG_IRQ    = LOCK_FLAG_IRQ;
 MESSAGESLOT_FLAG_FIQ    = LOCK_FLAG_FIQ;
 MESSAGESLOT_FLAG_IRQFIQ = LOCK_FLAG_IRQFIQ;

 {Mailslot constants}
 MAILSLOT_SIGNATURE = $7A409BF3;

 {Buffer constants}
 BUFFER_SIGNATURE = $830BEA71;

 {Buffer flag constants}
 BUFFER_FLAG_NONE    = $00000000;
 BUFFER_FLAG_SHARED  = $00000001;  {If set the buffer memory (Not the buffer entry itself) is allocated from shared memory}

 {Event constants}
 EVENT_SIGNATURE = $903BA69D;

 {Event state constants}
 EVENT_STATE_UNSIGNALED = 0;
 EVENT_STATE_SIGNALED   = 1;

 {Event flag constants}
 EVENT_FLAG_NONE           = $00000000;
 EVENT_FLAG_INITIAL_STATE  = $00000001;
 EVENT_FLAG_MANUAL_RESET   = $00000002;

 {Timer constants}
 TIMER_SIGNATURE = $AB7E07FB;

 {Timer state constants}
 TIMER_STATE_DISABLED = 0;
 TIMER_STATE_ENABLED  = 1;

 {Timer flag constants}
 TIMER_FLAG_NONE       = $00000000;
 TIMER_FLAG_RESCHEDULE = $00000001;     {Timer should be rescheduled each time the event completes}
 TIMER_FLAG_IMMEDIATE  = $00000002;     {Timer event should be executed immediately and then each interval milliseconds}
 TIMER_FLAG_WORKER     = $00000004;     {Timer event should be executed by a worker thread instead of a timer thread}
 TIMER_FLAG_PRIORITY   = $00000008;     {Timer event should be executed by a priority timer thread}

 {Timer key constants}
 TIMER_KEY_NONE = Integer($7FFFFFFF);   {Null key value returned from an empty Timer list}
 TIMER_KEY_MAX  = Integer($7FFFFFFE);   {Max key that can be ordered in a Timer list}
 TIMER_KEY_MIN  = Integer($80000000);   {Min key that can be ordered in a Timer list}

 {Worker constants}
 WORKER_SIGNATURE = $EF6A901B;

 {Worker flag constants}
 WORKER_FLAG_NONE       = $00000000;
 WORKER_FLAG_RESCHEDULE = $00000001;    {Worker task should be rescheduled each time the task completes}
 WORKER_FLAG_IMMEDIATE  = $00000002;    {Worker task should be executed immediately and then each interval milliseconds}
 WORKER_FLAG_CANCEL     = $00000004;    {Internal flag to indicate the worker task should be cancelled next time the interval expires}
 WORKER_FLAG_NOFREE     = $00000008;    {Internal flag to tell worker execute not to free the worker request when it is completed}
 WORKER_FLAG_TERMINATE  = $00000010;    {Internal flag to tell worker execute to terminate the worker thread}
 WORKER_FLAG_IRQ        = $00000020;    {Internal flag to tell worker execute to free IRQ memory when the request is completed}
 WORKER_FLAG_FIQ        = $00000040;    {Internal flag to tell worker execute to free FIQ memory when the request is completed}
 WORKER_FLAG_PRIORITY   = $00000080;    {Worker task should be executed by a priority worker thread}

 WORKER_FLAG_INTERNAL = WORKER_FLAG_CANCEL or WORKER_FLAG_NOFREE or WORKER_FLAG_TERMINATE or WORKER_FLAG_IRQ or WORKER_FLAG_FIQ; {Internal only flags}

 WORKER_FLAG_EXCLUDED_IRQ = WORKER_FLAG_RESCHEDULE or WORKER_FLAG_IMMEDIATE; {Excluded flags}
 WORKER_FLAG_EXCLUDED_FIQ = WORKER_FLAG_RESCHEDULE or WORKER_FLAG_IMMEDIATE; {Excluded flags}

 {Tasker task constants}
 TASKER_TASK_THREADSENDMESSAGE  = 1;  {Perform a ThreadSendMessage() function using the tasker list}
 TASKER_TASK_MESSAGESLOTSEND    = 2;  {Perform a MessageslotSend() function using the tasker list}
 TASKER_TASK_SEMAPHORESIGNAL    = 3;  {Perform a SemaphoreSignal() function using the tasker list}
 TASKER_TASK_COMPLETIONRESET    = 4;  {Perform a CompletionReset() function using the tasker list}
 TASKER_TASK_COMPLETIONCOMPLETE = 5;  {Perform a CompletionComplete() or CompletionCompleteAll() function using the tasker list}

 {Scheduler migration constants}
 SCHEDULER_MIGRATION_DISABLED = 0;
 SCHEDULER_MIGRATION_ENABLED  = 1;

 {Scheduler preempt constants}
 SCHEDULER_PREEMPT_DISABLED = 0;
 SCHEDULER_PREEMPT_ENABLED  = 1;

 {Scheduler allocation constants}
 SCHEDULER_ALLOCATION_DISABLED = 0;
 SCHEDULER_ALLOCATION_ENABLED  = 1;

 {Scheduler mask constants}
 SCHEDULER_MASK_NONE        = (1 shl THREAD_PRIORITY_NONE);
 SCHEDULER_MASK_IDLE        = (1 shl THREAD_PRIORITY_IDLE);
 SCHEDULER_MASK_LOWEST      = (1 shl THREAD_PRIORITY_LOWEST);
 SCHEDULER_MASK_LOWER       = (1 shl THREAD_PRIORITY_LOWER);
 SCHEDULER_MASK_NORMAL      = (1 shl THREAD_PRIORITY_NORMAL);
 SCHEDULER_MASK_HIGHER      = (1 shl THREAD_PRIORITY_HIGHER);
 SCHEDULER_MASK_HIGHEST     = (1 shl THREAD_PRIORITY_HIGHEST);
 SCHEDULER_MASK_CRITICAL    = (1 shl THREAD_PRIORITY_CRITICAL);

 SCHEDULER_MASKS:array[ 0..THREAD_PRIORITY_COUNT - 1] of LongWord = (
  SCHEDULER_MASK_NONE,
  SCHEDULER_MASK_IDLE,
  SCHEDULER_MASK_LOWEST,
  SCHEDULER_MASK_LOWER,
  SCHEDULER_MASK_NORMAL,
  SCHEDULER_MASK_HIGHER,
  SCHEDULER_MASK_HIGHEST,
  SCHEDULER_MASK_CRITICAL);

 {Scheduler quantum constants}
 SCHEDULER_QUANTUM_NONE     = 0;
 SCHEDULER_QUANTUM_IDLE     = 0;
 SCHEDULER_QUANTUM_LOWEST   = 1;
 SCHEDULER_QUANTUM_LOWER    = 2;
 SCHEDULER_QUANTUM_NORMAL   = 4;
 SCHEDULER_QUANTUM_HIGHER   = 6;
 SCHEDULER_QUANTUM_HIGHEST  = 8;
 SCHEDULER_QUANTUM_CRITICAL = 10;

 SCHEDULER_QUANTUMS:array[ 0..THREAD_PRIORITY_COUNT - 1] of LongWord = (
  SCHEDULER_QUANTUM_NONE,
  SCHEDULER_QUANTUM_IDLE,
  SCHEDULER_QUANTUM_LOWEST,
  SCHEDULER_QUANTUM_LOWER,
  SCHEDULER_QUANTUM_NORMAL,
  SCHEDULER_QUANTUM_HIGHER,
  SCHEDULER_QUANTUM_HIGHEST,
  SCHEDULER_QUANTUM_CRITICAL);

 {Thread logging}
 THREAD_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Thread debugging messages}
 THREAD_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Thread informational messages, such as a thread being created or destroyed}
 THREAD_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Thread warning messages}
 THREAD_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Thread error messages}
 THREAD_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Thread messages}

var
 THREAD_DEFAULT_LOG_LEVEL:LongWord = THREAD_LOG_LEVEL_DEBUG; {Minimum level for Thread messages.  Only messages with level greater than or equal to this will be printed}

var
 {Thread logging}
 THREAD_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {Thread specific types}

 {See also Handle types in GlobalConst}

 {Spin entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PSpinEntry = ^TSpinEntry;
 TSpinEntry = record
  {Spin Properties}
  Signature:LongWord;  {Signature for entry validation}
  State:LongWord;      {State of the lock (Locked/Unlocked)}
  Mask:LongWord;       {IRQ/FIQ Mask for Save/Restore}
  Owner:TThreadHandle; {Currently owning thread (or INVALID_HANDLE_VALUE if not locked)}
  {Internal Properties}
  Prev:PSpinEntry;     {Previous entry in Spin table}
  Next:PSpinEntry;     {Next entry in Spin table}
  {Statistics Properties}
 end;

 {Mutex entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PMutexEntry = ^TMutexEntry;
 TMutexEntry = record
  {Mutex Properties}
  Signature:LongWord;  {Signature for entry validation}
  State:LongWord;      {State of the lock (Locked/Unlocked)}
  Owner:TThreadHandle; {Currently owning thread (or INVALID_HANDLE_VALUE if not locked)}
  Yield:TThreadYield;  {Yield function to call while waiting}
  Count:LongWord;      {Count of lock recursions (only if Flags includes MUTEX_FLAG_RECURSIVE)}
  Flags:LongWord;      {Mutex Flags (eg MUTEX_FLAG_RECURSIVE)}
  SpinCount:LongWord;  {Number of times to spin wait for lock before Yield is called (Always 0 if SCHEDULER_CPU_COUNT = 1)}
  {Internal Properties}
  Prev:PMutexEntry;    {Previous entry in Mutex table}
  Next:PMutexEntry;    {Next entry in Mutex table}
  {Statistics Properties}
 end;

 {Critical Section entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PCriticalSectionEntry = ^TCriticalSectionEntry;
 TCriticalSectionEntry = record
  {Critical Section Properties}
  Signature:LongWord;         {Signature for entry validation}
  State:LongWord;             {State of the lock (Locked/Unlocked)}
  Count:LongWord;             {Count of lock recursions}
  Owner:TThreadHandle;        {Currently owning thread (or INVALID_HANDLE_VALUE if not locked)}
  SpinCount:LongWord;         {Number of times to spin wait for lock before Wait is called (Always 0 if SCHEDULER_CPU_COUNT = 1)}
  Lock:TSpinHandle;           {Critical Section Lock}
  List:TListHandle;           {List of threads waiting on this CriticalSection (or INVALID_HANDLE_VALUE if never used)}
  Wait:TThreadWait;           {Wait function to call to wait on the CriticalSection if it is already locked}
  WaitEx:TThreadWaitEx;       {Wait function to call to wait with timeout on the CriticalSection if it is already locked}
  Release:TThreadRelease;     {Release function to call if any threads are waiting when CriticalSection is unlocked}
  Abandon:TThreadAbandon;     {Abandon function to call if any threads are waiting when CriticalSection is destroyed}
  {Internal Properties}
  Prev:PCriticalSectionEntry; {Previous entry in CriticalSection table}
  Next:PCriticalSectionEntry; {Next entry in CriticalSection table}
  {Statistics Properties}
 end;

 {Semaphore entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PSemaphoreEntry = ^TSemaphoreEntry;
 TSemaphoreEntry = record
  {Semaphore Properties}
  Signature:LongWord;         {Signature for entry validation}
  Count:LongWord;             {Count for this Semaphore (Can be negative when Threads are waiting)}
  Maximum:LongWord;           {Maximum count for this Semaphore}
  Flags:LongWord;             {Semaphore Flags (eg SEMAPHORE_FLAG_IRQ)}
  Lock:TSpinHandle;           {Semaphore Lock}
  List:TListHandle;           {List of threads waiting on this Semaphore (or INVALID_HANDLE_VALUE if never used)}
  Wait:TThreadWait;           {Wait function to call to wait on the Semaphore if the count is equal to zero on SemaphoreWait}
  WaitEx:TThreadWaitEx;       {Wait function to call to wait with timeout on the Semaphore if the count is equal to zero on SemaphoreWait}
  Release:TThreadRelease;     {Release function to call if any threads are waiting on SemaphoreSignal}
  Abandon:TThreadAbandon;     {Abandon function to call if any threads are waiting when SemaphoreSignal is destroyed}
  {Internal Properties}
  Prev:PSemaphoreEntry;       {Previous entry in Semaphore table}
  Next:PSemaphoreEntry;       {Next entry in Semaphore table}
  {Statistics Properties}
 end;

 {Synchronizer entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PSynchronizerEntry = ^TSynchronizerEntry;
 TSynchronizerEntry = record
  {Synchronizer Properties}
  Signature:LongWord;         {Signature for entry validation}
  State:LongWord;             {State of the lock (Locked/Unlocked)}
  Lock:TSpinHandle;           {Synchronizer Lock}
  ReaderCount:LongWord;       {Count of current reader locks}
  WriterCount:LongWord;       {Count of current writer lock recursions}
  ReaderLast:TThreadHandle;   {Last thread to acquire reader lock (or INVALID_HANDLE_VALUE if no reader)}
  WriterOwner:TThreadHandle;  {Currently owning writer thread (or INVALID_HANDLE_VALUE if no writer)}
  ReaderList:TListHandle;     {List of reader threads waiting on this Synchronizer (or INVALID_HANDLE_VALUE if never used)}
  WriterList:TListHandle;     {List of writer threads waiting on this Synchronizer (or INVALID_HANDLE_VALUE if never used)}
  Wait:TThreadWait;           {Wait function to call to wait on the Synchronizer if it is already locked}
  WaitEx:TThreadWaitEx;       {Wait function to call to wait with timeout on the Synchronizer if it is already locked}
  Release:TThreadRelease;     {Release function to call if any threads are waiting when Synchronizer is unlocked}
  Abandon:TThreadAbandon;     {Abandon function to call if any threads are waiting when Synchronizer is destroyed}
  {Internal Properties}
  Prev:PSynchronizerEntry;    {Previous entry in Synchronizer table}
  Next:PSynchronizerEntry;    {Next entry in Synchronizer table}
  {Statistics Properties}
 end;

 {Condition entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PConditionEntry = ^TConditionEntry;
 TConditionEntry = record
  {Condition Properties}
  Signature:LongWord;         {Signature for entry validation}
  Flags:LongWord;             {Condition Flags (eg CONDITION_FLAG_NONE)}
  Lock:TSpinHandle;           {Condition Lock}
  List:TListHandle;           {List of threads waiting on this Condition (or INVALID_HANDLE_VALUE if never used)}
  Wait:TThreadWait;           {Wait function to call to wait on the Condition}
  WaitEx:TThreadWaitEx;       {Wait function to call to wait with timeout on the Condition}
  Release:TThreadRelease;     {Release function to call if any threads are waiting when Condition is woken}
  Abandon:TThreadAbandon;     {Abandon function to call if any threads are waiting when Condition is destroyed}
  {Internal Properties}
  Prev:PConditionEntry;       {Previous entry in Condition table}
  Next:PConditionEntry;       {Next entry in Condition table}
  {Statistics Properties}
 end;

 {Completion entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PCompletionEntry = ^TCompletionEntry;
 TCompletionEntry = record
  {Completion Properties}
  Signature:LongWord;         {Signature for entry validation}
  State:LongWord;             {State of the completion (Reset/Complete)}
  Count:LongWord;             {Count of the completion (Only applicable if COMPLETION_FLAG_COUNTED)}
  Flags:LongWord;             {Completion Flags (eg COMPLETION_FLAG_IRQ)}
  Lock:TSpinHandle;           {Completion Lock}
  List:TListHandle;           {List of threads waiting on this Completion (or INVALID_HANDLE_VALUE if never used)}
  Wait:TThreadWait;           {Wait function to call to wait on the Completion}
  WaitEx:TThreadWaitEx;       {Wait function to call to wait with timeout on the Completion}
  Release:TThreadRelease;     {Release function to call if any threads are waiting when Completion is completed}
  Abandon:TThreadAbandon;     {Abandon function to call if any threads are waiting when Completion is destroyed}
  {Internal Properties}
  Prev:PCompletionEntry;       {Previous entry in Completion table}
  Next:PCompletionEntry;       {Next entry in Completion table}
  {Statistics Properties}
 end;

 {List entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PListElement = ^TListElement;
 PListEntry = ^TListEntry;
 TListEntry = record
  {List Properties}
  Signature:LongWord;         {Signature for entry validation}
  ListType:LongWord;          {Type of this List (eg LIST_TYPE_WAIT_SECTION)}
  Count:LongWord;             {Count of items currently in this List}
  Flags:LongWord;             {List Flags (eg LIST_FLAG_IRQ)}
  Lock:TSpinHandle;           {List Lock}
  First:PListElement;         {First element in List}
  Last:PListElement;          {Last element in List}
  {Internal Properties}
  Prev:PListEntry;            {Previous entry in List table}
  Next:PListEntry;            {Next entry in List table}
  {Statistics Properties}
 end;

 {List element}
 TListElement = record
  Thread:TThreadHandle;       {The thread referenced by this List element}
  Prev:PListElement;          {Previous element in List}
  Next:PListElement;          {Next element in List}
 end;

 {List handles}
 PListHandles = ^TListHandles;
 TListHandles = array[0..THREAD_LISTS_MAXIMUM - 1] of TListHandle;

 {Queue entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PQueueElement = ^TQueueElement;
 PQueueEntry = ^TQueueEntry;
 TQueueEntry = record
  {Queue Properties}
  Signature:LongWord;         {Signature for entry validation}
  QueueType:LongWord;         {Type of this Queue (eg QUEUE_TYPE_SCHEDULE_IDLE)}
  Count:LongWord;             {Count of items currently in this Queue}
  Flags:LongWord;             {Queue Flags (eg QUEUE_FLAG_IRQ)}
  Lock:TSpinHandle;           {Queue Lock}
  First:PQueueElement;        {First element in Queue}
  Last:PQueueElement;         {Last element in Queue}
  {Internal Properties}
  Prev:PQueueEntry;           {Previous entry in Queue table}
  Next:PQueueEntry;           {Next entry in Queue table}
  {Statistics Properties}
 end;

 {Queue element}
 TQueueElement = record
  Key:Integer;                {Ordering key for Queue}
  Thread:TThreadHandle;       {The thread referenced by this Queue element}
  Prev:PQueueElement;         {Previous element in Queue}
  Next:PQueueElement;         {Next element in Queue}
 end;

 {Message list}
 PMessage = ^TMessage;
 PMessageList = ^TMessageList;
 TMessageList = record
  {Header Properties}
  Maximum:LongWord;           {Maximum number of messages in list}
  Count:LongWord;             {Current number of messages in list}
  Start:LongWord;             {First message available in list}
  {Internal Properties}
  List:PMessage;              {Message list}
  {Statistics Properties}
 end;

 {Message}
 TMessage = record
  Msg:PtrUInt;
  wParam:PtrUInt;
  lParam:PtrInt;
  Time:LongWord;
 end;

 {Thread entry}
 {Note: Changes to this structure need to be accounted for in platform specific handlers}
 PThreadEntry = ^TThreadEntry;
 TThreadEntry = record
  {Thread Properties}
  Signature:LongWord;                        {Signature for entry validation}
  State:LongWord;                            {State of the Thread (eg THREAD_STATE_RUNNING)}
  Flags:LongWord;                            {Flags of the Thread (eg THREAD_FLAG_PERSIST)}
  Priority:LongWord;                         {Priority of the Thread (eg THREAD_PRIORITY_NORMAL)}
  Affinity:LongWord;                         {CPU Affinity mask of the Thread}
  StackBase:Pointer;                         {Base (Top) of the thread stack}
  StackSize:LongWord;                        {Stack length in bytes}
  Name:array[0..THREAD_NAME_LENGTH - 1] of Char; {The name of the Thread}
  Lock:TSpinHandle;                          {Thread Lock}
  Parent:TThreadHandle;                      {Handle of the parent thread}
  Messages:TMessageList;                     {Messages sent to this thread}
  TlsPointer:Pointer;                        {Thread Local Storage Memory (RTL ThreadVars)}
  TlsTable:array[0..THREAD_TLS_MAXIMUM - 1] of Pointer;  {Thread Local Storage Index Pointers (for ThreadGetTlsValue/ThreadSetTlsValue)}
  TlsFlags:array[0..THREAD_TLS_MAXIMUM - 1] of LongWord; {Thread Local Storage Index Flags (eg THREAD_TLS_FLAG_FREE)}
  ExitCode:LongWord;                         {Thread Exit Code}
  LastError:LongWord;                        {Thread Last Error}
  Locale:LCID;                               {Thread Locale}
  {Internal Properties}
  CurrentCPU:LongWord;                       {Saved current CPU from last ContextSwitch}
  StackPointer:Pointer;                      {Saved current stack pointer from last ContextSwitch}
  TargetCPU:LongWord;                        {Target CPU of the Thread for next ContextSwitch}
  TargetPriority:LongWord;                   {Target Priority of the Thread for next ContextSwitch (eg THREAD_PRIORITY_NORMAL)}
  List:TListHandle;                          {List of threads waiting on this thread (or INVALID_HANDLE_VALUE if never used)}
  WaitList:TListHandle;                      {The wait list this thread is currently in (or INVALID_HANDLE_VALUE)}
  WaitLists:PListHandles;                    {The wait lists this thread is currently in if doing a multiple wait (or nil)}
  WaitResult:LongWord;                       {The result code for the last wait with timeout (eg WAIT_TIMEOUT)}
  ReceiveResult:LongWord;                    {The result code for the last receive with timeout (eg WAIT_TIMEOUT)}
  ScheduleQueue:TQueueHandle;                {The scheduler queue this thread is currently in  (or INVALID_HANDLE_VALUE)}
  ListElement:TListElement;                  {List element for this thread when in a Wait List}
  QueueElement:TQueueElement;                {Queue element for this thread when in a Schedule Queue}
  Prev:PThreadEntry;                         {Previous entry in Thread table}
  Next:PThreadEntry;                         {Next entry in Thread table}
  {Statistics Properties}
  CreateTime:Int64;                          {The time when this thread was created}
  ExitTime:Int64;                            {The time when this thread exited or was terminated}
  KernelTime:Int64;                          {The total amount of time this thread has been in the running state (ie CPU time consumed)}
  SwitchCount:Int64;                         {The number of times this thread has been selected to run by a context switch}
 end;

 {Thread snapshot} {Data returned by ThreadSnapshotCreate}
 PThreadSnapshot = ^TThreadSnapshot;
 TThreadSnapshot = record
  {Snapshot Properties}
  Handle:TThreadHandle;                      {Handle of the thread}
  State:LongWord;                            {State of the Thread (eg THREAD_STATE_RUNNING)}
  Flags:LongWord;                            {Flags of the Thread (eg THREAD_FLAG_PERSIST)}
  CPU:LongWord;                              {CPU from last ContextSwitch}
  Priority:LongWord;                         {Priority of the Thread (eg THREAD_PRIORITY_NORMAL)}
  Affinity:LongWord;                         {CPU Affinity mask of the Thread}
  StackBase:Pointer;                         {Base (Top) of the thread stack}
  StackSize:LongWord;                        {Stack length in bytes}
  StackPointer:Pointer;                      {Stack pointer from last ContextSwitch}
  Name:array[0..THREAD_NAME_LENGTH - 1] of Char; {The name of the Thread}
  Parent:TThreadHandle;                      {Handle of the parent thread}
  ExitCode:LongWord;                         {Thread Exit Code}
  LastError:LongWord;                        {Thread Last Error}
  Locale:LCID;                               {Thread Locale}
  TargetCPU:LongWord;                        {Target CPU of the Thread for next ContextSwitch}
  TargetPriority:LongWord;                   {Target Priority of the Thread for next ContextSwitch (eg THREAD_PRIORITY_NORMAL)}
  CreateTime:Int64;                          {The time when this thread was created}
  ExitTime:Int64;                            {The time when this thread exited or was terminated}
  KernelTime:Int64;                          {The total amount of time this thread has been in the running state (ie CPU time consumed)}
  SwitchCount:Int64;                         {The number of times this thread has been selected to run by a context switch}
  {Internal Properties}
  Next:PThreadSnapshot;                      {Next entry in Thread snapshot}
 end;

 {Messageslot entry}
 PMessageslotEntry = ^TMessageslotEntry;
 TMessageslotEntry = record
  {Messageslot Properties}
  Signature:LongWord;            {Signature for entry validation}
  Flags:LongWord;                {Messageslot Flags (eg MESSAGESLOT_FLAG_IRQ)}
  Lock:TSpinHandle;              {Messageslot Lock}
  List:TListHandle;              {List of threads waiting on this Messageslot (or INVALID_HANDLE_VALUE if never used)}
  Messages:TMessageList;         {Messageslot message queue}
  Wait:TThreadWait;              {Wait function to call to wait on the Messageslot if there are no messages}
  WaitEx:TThreadWaitEx;          {Wait function to call to wait with timeout on the Messageslot if there are no messages}
  Release:TThreadRelease;        {Release function to call if any threads are waiting when a message is sent}
  Abandon:TThreadAbandon;        {Abandon function to call if any threads are waiting when Messageslot is destroyed}
  {Internal Properties}
  Prev:PMessageslotEntry;        {Previous entry in Messageslot table}
  Next:PMessageslotEntry;        {Next entry in Messageslot table}
  {Statistics Properties}
 end;

 {Mailslot entry}
 PMailslotEntry = ^TMailslotEntry;
 TMailslotEntry = record
  {Mailslot Properties}
  Signature:LongWord;         {Signature for entry validation}
  Maximum:LongWord;           {Maximum number of messages in Mailslot}
  Count:LongWord;             {Current number of messages in Mailslot}
  Start:LongWord;             {First message available in Mailslot}
  Lock:TSpinHandle;           {Mailslot Lock}
  Sender:TSemaphoreHandle;    {Mailslot send Semaphore}
  Receiver:TSemaphoreHandle;  {Mailslot receive Semaphore}
  Messages:PPtrInt;           {Mailslot message queue}
  {Internal Properties}
  Prev:PMailslotEntry;        {Previous entry in Mailslot table}
  Next:PMailslotEntry;        {Next entry in Mailslot table}
  {Statistics Properties}
 end;

 {Buffer entry}
 PBufferItem = ^TBufferItem;
 PBufferEntry = ^TBufferEntry;
 TBufferEntry = record
  {Buffer Properties}
  Signature:LongWord;         {Signature for entry validation}
  Size:LongWord;              {Size of each buffer}
  Count:LongWord;             {Number of buffers}
  Flags:LongWord;             {Buffer Flags (eg BUFFER_FLAG_SHARED)}
  Lock:TSpinHandle;           {Buffer Lock}
  Available:TSemaphoreHandle; {Buffer available Semaphore}
  Buffers:PBufferItem;        {Buffer list}
  {Internal Properties}
  Prev:PBufferEntry;          {Previous entry in Buffer table}
  Next:PBufferEntry;          {Next entry in Buffer table}
  First:PBufferItem;          {First available buffer item}
  {Statistics Properties}
 end;

 {Buffer item}
 TBufferItem = record
  Parent:TBufferHandle;       {Handle of Buffer owning this item}
  Next:PBufferItem;           {Next item in list}
  Buffer:Pointer;             {Pointer to item data}
  Reserved:LongWord;          {Align to 16 bytes}
 end;

 {Event entry}
 PEventEntry = ^TEventEntry;
 TEventEntry = record
  {Event Properties}
  Signature:LongWord;         {Signature for entry validation}
  State:LongWord;             {State of the event (Signaled/Unsignaled)}
  Flags:LongWord;             {Event Flags (eg EVENT_FLAG_MANUAL)}
  Lock:TSpinHandle;           {Event Lock}
  List:TListHandle;           {List of threads waiting on this Event (or INVALID_HANDLE_VALUE if never used)}
  Wait:TThreadWait;           {Wait function to call to wait on the Event if it is not Signaled}
  WaitEx:TThreadWaitEx;       {Wait function to call to wait with timeout on the Event if it is not Signaled}
  Release:TThreadRelease;     {Release function to call if any threads are waiting when Event is Signaled}
  Abandon:TThreadAbandon;     {Abandon function to call if any threads are waiting when Event is destroyed}
  {Internal Properties}
  Prev:PEventEntry;           {Previous entry in Event table}
  Next:PEventEntry;           {Next entry in Event table}
  {Statistics Properties}
 end;

 {Timer list}
 PTimerItem = ^TTimerItem;
 PTimerList = ^TTimerList;
 TTimerList = record
  {List Properties}
  Count:LongWord;             {Count of items currently in the Timer list}
  Flags:LongWord;             {Timer list Flags (eg LOCK_FLAG_IRQ)}
  Lock:TSpinHandle;           {Timer list Lock}
  First:PTimerItem;           {First item in Timer list}
  Last:PTimerItem;            {Last item in Timer list}
  {Internal Properties}
  {Statistics Properties}
 end;

 {Timer item}
 TTimerItem = record
  Key:Integer;                {Ordering key for Timer list}
  Timer:TTimerHandle;         {The timer referenced by this Timer list item}
  Prev:PTimerItem;            {Previous item in Timer list}
  Next:PTimerItem;            {Next item in Timer list}
 end;

 {Timer entry}
 PTimerEntry = ^TTimerEntry;
 TTimerEntry = record
  {Timer Properties}
  Signature:LongWord;         {Signature for entry validation}
  Interval:LongWord;          {Interval for timer (Milliseconds)}
  State:LongWord;             {State of the timer (Enabled/Disabled)}
  Flags:LongWord;             {Timer Flags (eg TIMER_FLAG_RESCHEDULE)}
  Lock:TSpinHandle;           {Timer Lock}
  Event:TTimerEvent;          {Function to call when timer triggers}
  Data:Pointer;               {Data to pass to function when timer triggers}
  {Internal Properties}
  TimerList:PTimerList;       {The timer list this timer is currently in (or nil)}
  TimerItem:TTimerItem;       {Timer list item for this timer when in a Timer list}
  Prev:PTimerEntry;           {Previous entry in Timer table}
  Next:PTimerEntry;           {Next entry in Timer table}
  {Statistics Properties}
 end;

 {Worker request}
 PWorkerRequest = ^TWorkerRequest;
 TWorkerRequest = record
  {Worker Properties}
  Signature:LongWord;         {Signature for entry validation}
  Interval:LongWord;          {Interval for worker (Milliseconds)}
  Flags:LongWord;             {Worker Flags (eg WORKER_FLAG_RESCHEDULE)}
  Lock:TSpinHandle;           {Worker Lock (or INVALID_HANDLE_VALUE if Interval is 0 and Flags is not WORKER_FLAG_RESCHEDULE)}
  Timer:TTimerHandle;         {Worker Timer (or INVALID_HANDLE_VALUE if Interval is 0)}
  Task:TWorkerTask;           {Task to call by worker}
  Data:Pointer;               {Data to pass to task}
  Callback:TWorkerCallback;   {Callback when task is completed}
 end;

 {Tasker list}
 PTaskerTask = ^TTaskerTask;
 PTaskerList = ^TTaskerList;
 TTaskerList = record
  {List Properties}
  Count:LongWord;             {Count of tasks currently in the Tasker list}
  Lock:TSpinHandle;           {Tasker list Lock}
  First:PTaskerTask;          {First task in Tasker list}
  Last:PTaskerTask;           {Last task in Tasker list}
  {Internal Properties}
  {Statistics Properties}
 end;

 {Tasker task}
 TTaskerTask = record
  Task:LongWord;              {The task to be performed}
  Prev:PTaskerTask;           {Previous task in Tasker list}
  Next:PTaskerTask;           {Next task in Tasker list}
 end;

 {Tasker ThreadSendMessage task}
 PTaskerThreadSendMessage = ^TTaskerThreadSendMessage;
 TTaskerThreadSendMessage = record
  Task:LongWord;              {The task to be performed}
  Prev:PTaskerTask;           {Previous task in Tasker list}
  Next:PTaskerTask;           {Next task in Tasker list}
  {Internal Properties}
  Thread:TThreadHandle;       {Handle of the thread to send a message to}
  Message:TMessage;           {Message to send to the thread}
 end;

 {Tasker MessageslotSend task}
 PTaskerMessageslotSend = ^TTaskerMessageslotSend;
 TTaskerMessageslotSend = record
  Task:LongWord;                  {The task to be performed}
  Prev:PTaskerTask;               {Previous task in Tasker list}
  Next:PTaskerTask;               {Next task in Tasker list}
  {Internal Properties}
  Messageslot:TMessageslotHandle; {Handle of the message slot to send to}
  Message:TMessage;               {Message to be sent}
 end;

 {Tasker SemaphoreSignal task}
 PTaskerSemaphoreSignal = ^TTaskerSemaphoreSignal;
 TTaskerSemaphoreSignal = record
  Task:LongWord;              {The task to be performed}
  Prev:PTaskerTask;           {Previous task in Tasker list}
  Next:PTaskerTask;           {Next task in Tasker list}
  {Internal Properties}
  Semaphore:TSemaphoreHandle; {Handle of the semaphore to signal}
  Count:LongWord;             {The count to be signalled}
 end;

 {Tasker CompletionReset task}
 PTaskerCompletionReset = ^TTaskerCompletionReset;
 TTaskerCompletionReset = record
  Task:LongWord;                {The task to be performed}
  Prev:PTaskerTask;             {Previous task in Tasker list}
  Next:PTaskerTask;             {Next task in Tasker list}
  {Internal Properties}
  Completion:TCompletionHandle; {Handle of the completion to reset}
 end;

 {Tasker CompletionComplete task}
 PTaskerCompletionComplete = ^TTaskerCompletionComplete;
 TTaskerCompletionComplete = record
  Task:LongWord;                {The task to be performed}
  Prev:PTaskerTask;             {Previous task in Tasker list}
  Next:PTaskerTask;             {Next task in Tasker list}
  {Internal Properties}
  Completion:TCompletionHandle; {Handle of the completion to complete or complete all}
  All:LongBool;                 {False for complete, True for complete all}
 end;

type
 {RTL Thread Manager Types}
 PThreadInfo = ^TThreadInfo;
 TThreadInfo = record
  ThreadFunction:TThreadFunc;
  ThreadParameter:Pointer;
  StackLength:Cardinal;
 end;

type
 {Prototypes for CPU Start Handlers}
 TCPUStart  = procedure(CPUID:LongWord);

 type
 {Prototypes for Initialization Handlers}
 TPrimaryInit = procedure;
 TSchedulerInit = procedure;
 TSchedulerStart = procedure(CPUID:LongWord);
 TSecondaryInit = procedure;
 TSecondaryBoot = procedure(CPUID:LongWord);

type
 {Prototypes for Spin Lock/Unlock Handlers}
 TSpinLock = function(Spin:PSpinEntry):LongWord;
 TSpinUnlock = function(Spin:PSpinEntry):LongWord;

 TSpinLockIRQ = function(Spin:PSpinEntry):LongWord;
 TSpinUnlockIRQ = function(Spin:PSpinEntry):LongWord;

 TSpinLockFIQ = function(Spin:PSpinEntry):LongWord;
 TSpinUnlockFIQ = function(Spin:PSpinEntry):LongWord;

 TSpinLockIRQFIQ = function(Spin:PSpinEntry):LongWord;
 TSpinUnlockIRQFIQ = function(Spin:PSpinEntry):LongWord;

 TSpinCheckIRQ = function(Spin:PSpinEntry):Boolean;
 TSpinCheckFIQ = function(Spin:PSpinEntry):Boolean;

 TSpinExchangeIRQ = function(Spin1,Spin2:PSpinEntry):LongWord;
 TSpinExchangeFIQ = function(Spin1,Spin2:PSpinEntry):LongWord;

type
 {Prototypes for Mutex Lock/Unlock/TryLock Handlers}
 TMutexLock = function(Mutex:PMutexEntry):LongWord;
 TMutexUnlock = function(Mutex:PMutexEntry):LongWord;
 TMutexTryLock = function(Mutex:PMutexEntry):LongWord;

type
 {Prototypes for CriticalSection Lock/Unlock/TryLock Handlers}
 TCriticalSectionLock = function(CriticalSection:PCriticalSectionEntry):LongWord;
 TCriticalSectionLockEx = function(CriticalSection:PCriticalSectionEntry;Timeout:LongWord):LongWord;
 TCriticalSectionUnlock = function(CriticalSection:PCriticalSectionEntry):LongWord;
 TCriticalSectionTryLock = function(CriticalSection:PCriticalSectionEntry):LongWord;

type
 {Prototypes for Semaphore Wait/WaitEx/Signal Handlers}
 TSemaphoreWait = function(Semaphore:PSemaphoreEntry):LongWord;
 TSemaphoreWaitEx = function(Semaphore:PSemaphoreEntry;Timeout:LongWord):LongWord;
 TSemaphoreSignal = function(Semaphore:PSemaphoreEntry):LongWord;

type
 {Prototypes for Synchronizer Reader/WriterLock/Unlock Handlers}
 TSynchronizerLock = function(Synchronizer:PSynchronizerEntry):LongWord;
 TSynchronizerLockEx = function(Synchronizer:PSynchronizerEntry;Timeout:LongWord):LongWord;
 TSynchronizerUnlock = function(Synchronizer:PSynchronizerEntry):LongWord;
 TSynchronizerConvert = function(Synchronizer:PSynchronizerEntry):LongWord;
 TSynchronizerConvertEx = function(Synchronizer:PSynchronizerEntry;Timeout:LongWord):LongWord;

type
 {Prototypes for Condition Wait/Wake/WakeAll Handlers}
 TConditionWait = function(Condition:PConditionEntry;Timeout:LongWord):LongWord;
 TConditionWaitMutex = function(Condition:PConditionEntry;Mutex:TMutexHandle;Timeout:LongWord):LongWord;
 TConditionWaitSynchronizer = function(Condition:PConditionEntry;Synchronizer:TSynchronizerHandle;Flags,Timeout:LongWord):LongWord;
 TConditionWaitCriticalSection = function(Condition:PConditionEntry;CriticalSection:TCriticalSectionHandle;Timeout:LongWord):LongWord;
 TConditionWake = function(Condition:PConditionEntry):LongWord;
 TConditionWakeAll = function(Condition:PConditionEntry):LongWord;

type
 {Prototypes for Completion Wait/TryWait/Reset/Complete/CompleteAll Handlers}
 TCompletionWait = function(Completion:PCompletionEntry;Timeout:LongWord):LongWord;
 TCompletionTryWait = function(Completion:PCompletionEntry):LongWord;
 TCompletionReset = function(Completion:PCompletionEntry):LongWord;
 TCompletionComplete = function(Completion:PCompletionEntry):LongWord;
 TCompletionCompleteAll = function(Completion:PCompletionEntry):LongWord;

type
 {Prototypes for Messageslot Send/Receive Handlers}
 TMessageslotSend = function(Messageslot:PMessageslotEntry;Message:PMessage):LongWord;
 TMessageslotReceive = function(Messageslot:PMessageslotEntry;Message:PMessage):LongWord;
 TMessageslotReceiveEx = function(Messageslot:PMessageslotEntry;Message:PMessage;Timeout:LongWord):LongWord;

type
 {Prototypes for Mailslot Send/Receive Handlers}
 TMailslotSend = function(Mailslot:PMailslotEntry;Data:PtrInt):LongWord;
 TMailslotSendEx = function(Mailslot:PMailslotEntry;Data:PtrInt;Timeout:LongWord):LongWord;
 TMailslotReceive = function(Mailslot:PMailslotEntry):PtrInt;
 TMailslotReceiveEx = function(Mailslot:PMailslotEntry;Timeout:LongWord):PtrInt;

type
 {Prototypes for Buffer Get/GetEx/Free Handlers}
 TBufferGet = function(Buffer:PBufferEntry):Pointer;
 TBufferGetEx = function(Buffer:PBufferEntry;Timeout:LongWord):Pointer;
 TBufferFree = function(Buffer:Pointer):LongWord;

 TBufferIterate = function(Buffer:PBufferEntry;Previous:Pointer):Pointer;

type
 {Prototypes for Event Wait/WaitEx/Set/Reset/Pulse Handlers}
 TEventWait = function(Event:PEventEntry):LongWord;
 TEventWaitEx = function(Event:PEventEntry;Timeout:LongWord):LongWord;
 TEventSet = function(Event:PEventEntry):LongWord;
 TEventReset = function(Event:PEventEntry):LongWord;
 TEventPulse = function(Event:PEventEntry):LongWord;

type
 {Prototypes for Timer Enable/Disable/Check/Trigger Handlers}
 TTimerEnable = function(Timer:PTimerEntry):LongWord;
 TTimerEnableEx = function(Timer:PTimerEntry;Interval:LongWord;Event:TTimerEvent;Data:Pointer):LongWord;
 TTimerDisable = function(Timer:PTimerEntry):LongWord;
 TTimerCheck = function:LongWord;
 TTimerTrigger = function:LongWord;

type
 {Prototypes for Tasker Check/Trigger Handlers}
 TTaskerCheck = function:LongWord;
 TTaskerTrigger = function:LongWord;

{type}
 {Prototypes for Thread Yield/Wait/WaitEx/Release/Wake Handlers}
 {Defined in Platform}

type
 {Prototype for Thread Get/SetCurrent Handlers}
 TThreadGetCurrent = function:TThreadHandle;
 TThreadSetCurrent = function(Thread:TThreadHandle):LongWord;

type
 {Prototypes for Thread Start/End Handlers}
 TThreadStart = function(Parameter:Pointer):PtrInt;{$IFDEF i386} stdcall;{$ENDIF}
 TThreadEnd = procedure(ExitCode:LongWord);{$IFDEF i386} stdcall;{$ENDIF}

type
 {Prototypes for Scheduler Check/Wakeup/Expire/Select/Switch Handlers}
 TSchedulerCheck = function(CPUID:LongWord):LongWord;
 TSchedulerWakeup = function(CPUID:LongWord):LongWord;
 TSchedulerExpire = function(CPUID:LongWord):LongWord;
 TSchedulerSwitch = function(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
 TSchedulerSelect = function(CPUID:LongWord;Thread:TThreadHandle;Yield:Boolean):TThreadHandle;
 TSchedulerReschedule = function (Yield:Boolean):LongWord;

 TSchedulerMigrationEnable = function:LongWord;
 TSchedulerMigrationDisable = function:LongWord;

 TSchedulerPreemptEnable = function(CPUID:LongWord):LongWord;
 TSchedulerPreemptDisable = function(CPUID:LongWord):LongWord;

 TSchedulerAllocationEnable = function(CPUID:LongWord):LongWord;
 TSchedulerAllocationDisable = function(CPUID:LongWord):LongWord;

type
 {Prototype for Thread Setup Stack Handler}
 TThreadSetupStack = function(StackBase:Pointer;StartProc:TThreadStart;ReturnProc:TThreadEnd;Parameter:Pointer):Pointer;

{==============================================================================}
{var}
 {Thread specific variables}

 //To Do //Critical //Can some or all of these be moved to implementation ?
var
 {Scheduler Variables}
 SchedulerThreadNext:LongWord;                    {The CPU to assign the next created thread to, round robin incremented on each create (Protected by ThreadTableLock)}
 SchedulerThreadMigration:LongWord;               {Enable or Disable thread migration (SCHEDULER_MIGRATION_DISABLED or SCHEDULER_MIGRATION_ENABLED}

 SchedulerMigrationQuantum:LongWord;              {Quantum for thread migration checks (CPU 0 only)}

 SchedulerThreadCount:array of LongWord;          {Current number of ready threads per CPU (One per CPU, allocated by scheduler initialization) (Protected by InterlockedIncrement/Decrement)}
 SchedulerThreadQuantum:array of LongWord;        {Quantum of current thread per CPU (One per CPU, allocated by scheduler initialization)}
 SchedulerThreadPreempt:array of LongWord;        {Current state of thread preemption per CPU (eg SCHEDULER_PREEMPT_DISABLED) (One per CPU, allocated by scheduler initialization)}
 SchedulerThreadAllocation:array of LongWord;     {Current state of thread allocation per CPU (eg SCHEDULER_ALLOCATION_DISABLED) (One per CPU, allocated by scheduler initialization)}

 SchedulerPriorityMask:array of LongWord;         {Mask of ready threads at each priority level (One per CPU, allocated by scheduler initialization) (Protected by InterlockedOr/And)}

 {$IFNDEF SCHEDULER_YIELD_ALTERNATE}
 SchedulerYieldCurrent:array of LongBool;         {Scheduler yield to current priority level status (One per CPU, allocated by scheduler initialization)}
 {$ENDIF}
 SchedulerStarvationNext:array of LongWord;       {Scheduler starvation priority round robin (One per CPU, allocated by scheduler initialization)}
 SchedulerStarvationQuantum:array of LongWord;    {Quantum for thread starvation checks per CPU (One per CPU, allocated by scheduler initialization)}

 SchedulerNoneQueue:array of TQueueHandle;        {Queue of threads that are ready to run at priority level none (One per CPU, allocated by scheduler initialization)}
 SchedulerIdleQueue:array of TQueueHandle;        {Queue of threads that are ready to run at priority level idle (One per CPU, allocated by scheduler initialization)}
 SchedulerLowestQueue:array of TQueueHandle;      {Queue of threads that are ready to run at priority level lowest (One per CPU, allocated by scheduler initialization)}
 SchedulerLowerQueue:array of TQueueHandle;       {Queue of threads that are ready to run at priority level lower (One per CPU, allocated by scheduler initialization)}
 SchedulerNormalQueue:array of TQueueHandle;      {Queue of threads that are ready to run at priority level normal (One per CPU, allocated by scheduler initialization)}
 SchedulerHigherQueue:array of TQueueHandle;      {Queue of threads that are ready to run at priority level higher (One per CPU, allocated by scheduler initialization)}
 SchedulerHighestQueue:array of TQueueHandle;     {Queue of threads that are ready to run at priority level highest (One per CPU, allocated by scheduler initialization)}
 SchedulerCriticalQueue:array of TQueueHandle;    {Queue of threads that are ready to run at priority level critical (One per CPU, allocated by scheduler initialization)}

 SchedulerSleepQueue:array of TQueueHandle;       {Queue of threads that are currently sleeping (One per CPU, allocated by scheduler initialization)}
 SchedulerTimeoutQueue:array of TQueueHandle;     {Queue of threads that are currently waiting with a timeout (One per CPU, allocated by scheduler initialization)}
 SchedulerTerminationQueue:array of TQueueHandle; {Queue of threads that have been terminated (One per CPU, allocated by scheduler initialization)}

 SchedulerLast:array of LongWord;                 {The timer value of the last scheduler interrupt (One per CPU, allocated by scheduler initialization)}
 SchedulerInterrupts:array of LongWord;           {Current number of scheduler interrupts per CPU (When this reaches SCHEDULER_INTERRUPTS_PER_SECOND then UtilizationLast is updated and UtilizationCurrent is reset) (One per CPU, allocated by scheduler initialization)}

 {$IFDEF SCHEDULER_DEBUG}
 SchedulerInterruptCounter:array of Int64;
 SchedulerInterruptOffset:array of LongWord;
 SchedulerInterruptMinOffset:array of LongWord;
 SchedulerInterruptMaxOffset:array of LongWord;
 SchedulerInterruptRollover:array of LongWord;

 SchedulerSelectEntry:array of Int64;              {Number of times the scheduler select routine has been called (One per CPU)}
 SchedulerSelectYield:array of Int64;              {Number of times the scheduler select routine has been called with yield equal to true (One per CPU)}
 SchedulerSelectForce:array of Int64;              {Number of times the scheduler forced a thread switch due to starvation quantum (One per CPU)}
 SchedulerSelectNoMask:array of Int64;             {Number of times the scheduler selection encountered an empty priority mask (Should never happen)(One per CPU)}
 SchedulerSelectNormal:array of Int64;             {Number of times the scheduler selection used normal next highest priority to select thread (One per CPU)}
 SchedulerSelectInvalid:array of Int64;            {Number of times the scheduler selection resulted in INVALID_THREAD_HANDLE (Should never happen)(One per CPU)}
 SchedulerSelectFailure:array of Int64;            {Number of times the scheduler failed to enqueue the currently running thread (Should never happen)(One per CPU)}
 SchedulerSelectNoReady:array of Int64;            {Number of times the scheduler selection found no ready thread available to run (Should never happen)(One per CPU)}
 SchedulerSelectDefaulted:array of Int64;          {Number of times the scheduler selection defaulted to an IDLE or NONE thread (Should never happen)(One per CPU)}

 SchedulerStarvationReset:array of Int64;          {Number of times the scheduler reset the starvation quantum (One per CPU)} //To Do //Remove
 SchedulerStarvationDecrement:array of Int64;      {Number of times the scheduler decremented the starvation quantum (One per CPU)}

 SchedulerSelectCPU:array of Int64;                {Number of times the scheduler selection changed the CurrentCPU on a running thread (One per CPU)}
 SchedulerSelectPriority:array of Int64;           {Number of times the scheduler selection changed the Priority on a running thread (One per CPU)}
 SchedulerSelectAffinity:array of Int64;           {Number of times the scheduler selection changed the CurrentCPU on a running thread due to Affinity mismatch (One per CPU)}

 SchedulerSwitchEntry:array of Int64;              {Number of times the scheduler switch routine has been called (One per CPU)}
 SchedulerSwitchThread:array of TThreadHandle;     {The thread returned by scheduler select on the last scheduler switch call (One per CPU)}
 SchedulerSwitchCounter:array of Int64;            {Number of times the scheduler switch resulted in a thread switch (One per CPU)}
 SchedulerSwitchCurrent:array of Int64;            {Number of times the scheduler switch resulted in the current thread continuing (One per CPU)}
 SchedulerSwitchInvalid:array of Int64;            {Number of times the scheduler switch was returned INVALID_THREAD_HANDLE by scheduler select (Should never happen)(One per CPU)}

 SchedulerRescheduleEntry:array of Int64;          {Number of times the scheduler reschedule routine has been called (One per CPU)}
 SchedulerRescheduleThread:array of TThreadHandle; {The thread returned by scheduler select on the last scheduler reschedule call (One per CPU)}
 SchedulerRescheduleCounter:array of Int64;        {Number of times the scheduler reschedule resulted in a thread switch (One per CPU)}
 SchedulerRescheduleCurrent:array of Int64;        {Number of times the scheduler reschedule resulted in the current thread continuing (One per CPU)}
 SchedulerRescheduleInvalid:array of Int64;        {Number of times the scheduler reschedule was returned INVALID_THREAD_HANDLE by scheduler select (Should never happen)(One per CPU)}

 SchedulerMigrationCounter:Int64;                  {Number of times scheduler select invoked a thread migration (CPU 0 only)}

 SchedulerTerminationCounter:array of Int64;       {Number of threads destroyed by scheduler reschedule (After termination quantum)(One per CPU)}

 SchedulerSecondaryWaitCounter:array of Int64;     {Number of wait cycles performed by secondary CPUs while waiting for init completion (One per CPU)}
 {$ENDIF}

 {$IFDEF LOCK_DEBUG}
 SpinDeadlockCounter:Int64;
 SpinRecursionCounter:Int64;
 SpinRecursionThread:TThreadHandle;
 SpinIRQThreadCounter:Int64;
 SpinFIQThreadCounter:Int64;
 SpinSWIThreadCounter:Int64;
 SpinIdleThreadCounter:Int64;
 MutexDeadlockCounter:Int64;
 MutexRecursionCounter:Int64;
 MutexRecursionThread:TThreadHandle;
 MutexIRQThreadCounter:Int64;
 MutexFIQThreadCounter:Int64;
 MutexSWIThreadCounter:Int64;
 MutexIdleThreadCounter:Int64;
 CriticalSectionDeadlockCounter:Int64;
 SemaphoreDeadlockCounter:Int64;
 SynchronizerDeadlockCounter:Int64;
 SynchronizerRecursionCounter:Int64;
 ConditionDeadlockCounter:Int64;
 CompletionDeadlockCounter:Int64;
 MessageslotDeadlockCounter:Int64;
 MailslotDeadlockCounter:Int64;
 BufferDeadlockCounter:Int64;
 EventDeadlockCounter:Int64;
 {$ENDIF}

 {$IFDEF SPIN_DEBUG}
 SpinLockEntry:LongWord;
 SpinUnlockEntry:LongWord;

 SpinLockExit:LongWord;
 SpinUnlockExit:LongWord;

 SpinUnlockNoLock:LongWord;
 SpinUnlockNoOwner:LongWord;

 SpinLockCounter:LongWord;
 SpinUnlockCounter:LongWord;
 SpinDestroyCounter:LongWord;

 SpinLockIRQCounter:LongWord;
 SpinUnlockIRQCounter:LongWord;
 SpinLockFIQCounter:LongWord;
 SpinUnlockFIQCounter:LongWord;
 SpinLockIRQFIQCounter:LongWord;
 SpinUnlockIRQFIQCounter:LongWord;
 {$ENDIF}

 {$IFDEF MUTEX_DEBUG}
 MutexLockEntry:LongWord;
 MutexUnlockEntry:LongWord;

 MutexLockExit:LongWord;
 MutexUnlockExit:LongWord;

 MutexUnlockNoLock:LongWord;
 MutexUnlockNoOwner:LongWord;

 MutexLockCounter:LongWord;
 MutexUnlockCounter:LongWord;
 MutexDestroyCounter:LongWord;
 {$ENDIF}

var
 {Heap Manager Variables}
 HeapLock:THeapLock;

var
 {RTL Thread Manager Variables}
 ThreadVarBlockSize:DWORD;

var
 {Inititalization Handlers}
 PrimaryInitHandler:TPrimaryInit;
 SchedulerInitHandler:TSchedulerInit;
 SchedulerStartHandler:TSchedulerStart;
 SecondaryInitHandler:TSecondaryInit;
 SecondaryBootHandler:TSecondaryBoot;

var
 {SpinLock/Unlock Handlers}
 SpinLockHandler:TSpinLock;
 SpinUnlockHandler:TSpinUnlock;

 SpinLockIRQHandler:TSpinLockIRQ;
 SpinUnlockIRQHandler:TSpinUnlockIRQ;

 SpinLockFIQHandler:TSpinLockFIQ;
 SpinUnlockFIQHandler:TSpinUnlockFIQ;

 SpinLockIRQFIQHandler:TSpinLockIRQFIQ;
 SpinUnlockIRQFIQHandler:TSpinUnlockIRQFIQ;

 SpinCheckIRQHandler:TSpinCheckIRQ;
 SpinCheckFIQHandler:TSpinCheckFIQ;

 SpinExchangeIRQHandler:TSpinExchangeIRQ;
 SpinExchangeFIQHandler:TSpinExchangeFIQ;

var
 {MutexLock/Unlock Handlers}
 MutexLockHandler:TMutexLock;
 MutexUnlockHandler:TMutexUnlock;
 MutexTryLockHandler:TMutexTryLock;

var
 {CriticalSection Lock/Unlock/TryLock Handlers}
 CriticalSectionLockHandler:TCriticalSectionLock;
 CriticalSectionLockExHandler:TCriticalSectionLockEx;
 CriticalSectionUnlockHandler:TCriticalSectionUnlock;
 CriticalSectionTryLockHandler:TCriticalSectionTryLock;

var
 {Semaphore Wait/Signal Handlers}
 SemaphoreWaitHandler:TSemaphoreWait;
 SemaphoreWaitExHandler:TSemaphoreWaitEx;
 SemaphoreSignalHandler:TSemaphoreSignal;

var
 {Synchronizer Reader/WriterLock/Unlock Handlers}
 SynchronizerReaderLockHandler:TSynchronizerLock;
 SynchronizerWriterLockHandler:TSynchronizerLock;
 SynchronizerReaderLockExHandler:TSynchronizerLockEx;
 SynchronizerWriterLockExHandler:TSynchronizerLockEx;
 SynchronizerReaderUnlockHandler:TSynchronizerUnlock;
 SynchronizerWriterUnlockHandler:TSynchronizerUnlock;
 SynchronizerReaderConvertHandler:TSynchronizerConvert;
 SynchronizerWriterConvertHandler:TSynchronizerConvert;
 SynchronizerReaderConvertExHandler:TSynchronizerConvertEx;

var
 {Condition Wait/Wake/WakeAll Handlers}
 ConditionWaitHandler:TConditionWait;
 ConditionWaitMutexHandler:TConditionWaitMutex;
 ConditionWaitSynchronizerHandler:TConditionWaitSynchronizer;
 ConditionWaitCriticalSectionHandler:TConditionWaitCriticalSection;
 ConditionWakeHandler:TConditionWake;
 ConditionWakeAllHandler:TConditionWakeAll;

var
 {Completion Wait/TryWait/Reset/Complete/CompleteAll Handlers}
 CompletionWaitHandler:TCompletionWait;
 CompletionTryWaitHandler:TCompletionTryWait;
 CompletionResetHandler:TCompletionReset;
 CompletionCompleteHandler:TCompletionComplete;
 CompletionCompleteAllHandler:TCompletionCompleteAll;

var
 {Messageslot Send/Receive Handlers}
 MessageslotSendHandler:TMessageslotSend;
 MessageslotReceiveHandler:TMessageslotReceive;
 MessageslotReceiveExHandler:TMessageslotReceiveEx;

var
 {Mailslot Send/Receive Handlers}
 MailslotSendHandler:TMailslotSend;
 MailslotSendExHandler:TMailslotSendEx;
 MailslotReceiveHandler:TMailslotReceive;
 MailslotReceiveExHandler:TMailslotReceiveEx;

var
 {Buffer Get/GetEx/Free Handlers}
 BufferGetHandler:TBufferGet;
 BufferGetExHandler:TBufferGetEx;
 BufferFreeHandler:TBufferFree;

 BufferIterateHandler:TBufferIterate;

var
 {Event Wait/Set/Reset/Pulse Handlers}
 EventWaitHandler:TEventWait;
 EventWaitExHandler:TEventWaitEx;
 EventSetHandler:TEventSet;
 EventResetHandler:TEventReset;
 EventPulseHandler:TEventPulse;

var
 {Timer Enable/Disable/Check/Trigger Handlers}
 TimerEnableHandler:TTimerEnable;
 TimerEnableExHandler:TTimerEnableEx;
 TimerDisableHandler:TTimerDisable;
 TimerCheckHandler:TTimerCheck;
 TimerTriggerHandler:TTimerTrigger;

var
 {Tasker Check/Trigger Handlers}
 TaskerCheckHandler:TTaskerCheck;
 TaskerTriggerHandler:TTaskerTrigger;

var
 {Thread Get/SetCurrent Handlers}
 ThreadGetCurrentHandler:TThreadGetCurrent;
 ThreadSetCurrentHandler:TThreadSetCurrent;

var
 {Scheduler Check/Wakeup/Expire/Select/Switch Handlers}
 SchedulerCheckHandler:TSchedulerCheck;
 SchedulerWakeupHandler:TSchedulerWakeup;
 SchedulerExpireHandler:TSchedulerExpire;
 SchedulerSwitchHandler:TSchedulerSwitch;
 SchedulerSelectHandler:TSchedulerSelect;
 SchedulerRescheduleHandler:TSchedulerReschedule;

 SchedulerMigrationEnableHandler:TSchedulerMigrationEnable;
 SchedulerMigrationDisableHandler:TSchedulerMigrationDisable;

 SchedulerPreemptEnableHandler:TSchedulerPreemptEnable;
 SchedulerPreemptDisableHandler:TSchedulerPreemptDisable;

 SchedulerAllocationEnableHandler:TSchedulerAllocationEnable;
 SchedulerAllocationDisableHandler:TSchedulerAllocationDisable;

var
 {Thread SetupStack Handlers}
 ThreadSetupStackHandler:TThreadSetupStack;

{==============================================================================}
{External Declarations}
procedure Pascalmain; external name 'PASCALMAIN';

{==============================================================================}
{Initialization Functions}
procedure LocksInit;
procedure ThreadsInit;

procedure PrimaryInit;
procedure SchedulerInit;
procedure SchedulerStart(CPUID:LongWord);
procedure SecondaryInit;
procedure SecondaryBoot(CPUID:LongWord);
procedure SecondaryStart(CPUID:LongWord);

function IRQExecute(Parameter:Pointer):PtrInt;
function FIQExecute(Parameter:Pointer):PtrInt;
function SWIExecute(Parameter:Pointer):PtrInt;
function IdleExecute(Parameter:Pointer):PtrInt;
function MainExecute(Parameter:Pointer):PtrInt;
function TimerExecute(Parameter:Pointer):PtrInt;
function WorkerExecute(Parameter:Pointer):PtrInt;
function TimerPriorityExecute(Parameter:Pointer):PtrInt;
function WorkerPriorityExecute(Parameter:Pointer):PtrInt;

function IdleCalibrate:LongWord;

{==============================================================================}
{Spin Functions}
function SpinCreate:TSpinHandle; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
function SpinCreateEx(InitialOwner:Boolean):TSpinHandle;
function SpinDestroy(Spin:TSpinHandle):LongWord;

function SpinOwner(Spin:TSpinHandle):TThreadHandle;

function SpinLock(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
function SpinUnlock(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}

function SpinLockIRQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
function SpinUnlockIRQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}

function SpinLockFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
function SpinUnlockFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}

function SpinLockIRQFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
function SpinUnlockIRQFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}

function SpinLockPreempt(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
function SpinUnlockPreempt(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}

function SpinCheckIRQ(Spin:TSpinHandle):Boolean;
function SpinCheckFIQ(Spin:TSpinHandle):Boolean;

function SpinExchangeIRQ(Spin1,Spin2:TSpinHandle):LongWord;
function SpinExchangeFIQ(Spin1,Spin2:TSpinHandle):LongWord;

function SpinMaskExchange(Spin1,Spin2:TSpinHandle):LongWord; //Remove

{==============================================================================}
{Mutex Functions}
function MutexCreate:TMutexHandle; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}
function MutexCreateEx(InitialOwner:Boolean;SpinCount:LongWord;Flags:LongWord):TMutexHandle;
function MutexDestroy(Mutex:TMutexHandle):LongWord;

function MutexFlags(Mutex:TMutexHandle):LongWord;
function MutexCount(Mutex:TMutexHandle):LongWord;
function MutexOwner(Mutex:TMutexHandle):TThreadHandle;

function MutexLock(Mutex:TMutexHandle):LongWord; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}
function MutexUnlock(Mutex:TMutexHandle):LongWord; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}
function MutexTryLock(Mutex:TMutexHandle):LongWord; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}

{==============================================================================}
{Critical Section Functions}
function CriticalSectionCreate:TCriticalSectionHandle; {$IFDEF CRITICALSECTION_INLINE}inline;{$ENDIF}
function CriticalSectionCreateEx(InitialOwner:Boolean;SpinCount:LongWord):TCriticalSectionHandle;
function CriticalSectionDestroy(CriticalSection:TCriticalSectionHandle):LongWord;

function CriticalSectionCount(CriticalSection:TCriticalSectionHandle):LongWord;
function CriticalSectionOwner(CriticalSection:TCriticalSectionHandle):TThreadHandle;
function CriticalSectionSetSpinCount(CriticalSection:TCriticalSectionHandle;SpinCount:LongWord):LongWord;

function CriticalSectionLock(CriticalSection:TCriticalSectionHandle):LongWord;
function CriticalSectionLockEx(CriticalSection:TCriticalSectionHandle;Timeout:LongWord):LongWord; {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function CriticalSectionUnlock(CriticalSection:TCriticalSectionHandle):LongWord;
function CriticalSectionTryLock(CriticalSection:TCriticalSectionHandle):LongWord;

{==============================================================================}
{Semaphore Functions}
function SemaphoreCreate(Count:LongWord):TSemaphoreHandle; {$IFDEF SEMAPHORE_INLINE}inline;{$ENDIF}
function SemaphoreCreateEx(Count,Maximum:LongWord;Flags:LongWord):TSemaphoreHandle;
function SemaphoreDestroy(Semaphore:TSemaphoreHandle):LongWord;

function SemaphoreCount(Semaphore:TSemaphoreHandle):LongWord;

function SemaphoreWait(Semaphore:TSemaphoreHandle):LongWord;
function SemaphoreWaitEx(Semaphore:TSemaphoreHandle;Timeout:LongWord):LongWord;                 {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function SemaphoreSignal(Semaphore:TSemaphoreHandle):LongWord;
function SemaphoreSignalEx(Semaphore:TSemaphoreHandle;Count:LongWord;Previous:PLongWord):LongWord;

{==============================================================================}
{Synchronizer Functions}
function SynchronizerCreate:TSynchronizerHandle; {$IFDEF SYNCHRONIZER_INLINE}inline;{$ENDIF}
function SynchronizerCreateEx(InitialReader,InitialWriter:Boolean):TSynchronizerHandle;
function SynchronizerDestroy(Synchronizer:TSynchronizerHandle):LongWord;

function SynchronizerReaderCount(Synchronizer:TSynchronizerHandle):LongWord;
function SynchronizerReaderLast(Synchronizer:TSynchronizerHandle):TThreadHandle;

function SynchronizerReaderLock(Synchronizer:TSynchronizerHandle):LongWord;
function SynchronizerReaderLockEx(Synchronizer:TSynchronizerHandle;Timeout:LongWord):LongWord;  {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function SynchronizerReaderUnlock(Synchronizer:TSynchronizerHandle):LongWord;
function SynchronizerReaderConvert(Synchronizer:TSynchronizerHandle):LongWord;
function SynchronizerReaderConvertEx(Synchronizer:TSynchronizerHandle;Timeout:LongWord):LongWord; {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}

function SynchronizerWriterCount(Synchronizer:TSynchronizerHandle):LongWord;
function SynchronizerWriterOwner(Synchronizer:TSynchronizerHandle):TThreadHandle;

function SynchronizerWriterLock(Synchronizer:TSynchronizerHandle):LongWord;
function SynchronizerWriterLockEx(Synchronizer:TSynchronizerHandle;Timeout:LongWord):LongWord;  {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function SynchronizerWriterUnlock(Synchronizer:TSynchronizerHandle):LongWord;
function SynchronizerWriterConvert(Synchronizer:TSynchronizerHandle):LongWord;

{==============================================================================}
{Condition Functions}
function ConditionCreate:TConditionHandle;
function ConditionDestroy(Condition:TConditionHandle):LongWord;

function ConditionWait(Condition:TConditionHandle;Timeout:LongWord = INFINITE):LongWord;                                                             {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function ConditionWaitMutex(Condition:TConditionHandle;Mutex:TMutexHandle;Timeout:LongWord = INFINITE):LongWord;                                     {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function ConditionWaitSynchronizer(Condition:TConditionHandle;Synchronizer:TSynchronizerHandle;Flags:LongWord;Timeout:LongWord = INFINITE):LongWord; {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function ConditionWaitCriticalSection(Condition:TConditionHandle;CriticalSection:TCriticalSectionHandle;Timeout:LongWord = INFINITE):LongWord;       {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}

function ConditionWake(Condition:TConditionHandle):LongWord;
function ConditionWakeAll(Condition:TConditionHandle):LongWord;

{==============================================================================}
{Completion Functions}
function CompletionCreate(Flags:LongWord = COMPLETION_FLAG_NONE):TCompletionHandle;
function CompletionDestroy(Completion:TCompletionHandle):LongWord;

function CompletionState(Completion:TCompletionHandle):LongWord;

function CompletionWait(Completion:TCompletionHandle;Timeout:LongWord = INFINITE):LongWord;
function CompletionTryWait(Completion:TCompletionHandle):LongWord;

function CompletionReset(Completion:TCompletionHandle):LongWord;
function CompletionComplete(Completion:TCompletionHandle):LongWord;
function CompletionCompleteAll(Completion:TCompletionHandle):LongWord;

{==============================================================================}
{List Functions}
function ListCreate:TListHandle; {$IFDEF LIST_INLINE}inline;{$ENDIF}
function ListCreateEx(ListType:LongWord;Flags:LongWord):TListHandle;
function ListDestroy(List:TListHandle):LongWord;

function ListCount(List:TListHandle):LongWord;

function ListAddFirst(List:TListHandle;Element:PListElement):LongWord;
function ListAddLast(List:TListHandle;Element:PListElement):LongWord;

function ListGetThread(List:TListHandle;Thread:TThreadHandle):PListElement;

function ListGetFirst(List:TListHandle):PListElement; {$IFDEF LIST_INLINE}inline;{$ENDIF}
function ListGetFirstEx(List:TListHandle;Remove:Boolean):PListElement;
function ListGetLast(List:TListHandle):PListElement; {$IFDEF LIST_INLINE}inline;{$ENDIF}
function ListGetLastEx(List:TListHandle;Remove:Boolean):PListElement;

function ListInsert(List:TListHandle;Previous,Element:PListElement):LongWord;
function ListRemove(List:TListHandle;Element:PListElement):LongWord;

function ListIsEmpty(List:TListHandle):Boolean;
function ListNotEmpty(List:TListHandle):Boolean;

function ListLock(List:TListHandle):LongWord; {$IFDEF LIST_INLINE}inline;{$ENDIF}
function ListUnlock(List:TListHandle):LongWord; {$IFDEF LIST_INLINE}inline;{$ENDIF}

{==============================================================================}
{Queue Functions}
function QueueCreate:TQueueHandle; {$IFDEF QUEUE_INLINE}inline;{$ENDIF}
function QueueCreateEx(QueueType:LongWord;Flags:LongWord):TQueueHandle;
function QueueDestroy(Queue:TQueueHandle):LongWord;

function QueueCount(Queue:TQueueHandle):LongWord;

//function QueueFind(Queue:TQueueHandle;Affinity:LongWord):TThreadHandle; //To Do //Critical //For thread migrations

function QueueEnqueue(Queue:TQueueHandle;Thread:TThreadHandle):LongWord;
function QueueDequeue(Queue:TQueueHandle):TThreadHandle;

function QueueFirstKey(Queue:TQueueHandle):Integer;
function QueueLastKey(Queue:TQueueHandle):Integer;

function QueueInsertKey(Queue:TQueueHandle;Thread:TThreadHandle;Key:Integer):LongWord;
function QueueDeleteKey(Queue:TQueueHandle;Thread:TThreadHandle):LongWord;
function QueueIncrementKey(Queue:TQueueHandle):Integer;
function QueueDecrementKey(Queue:TQueueHandle):Integer;

function QueueIsEmpty(Queue:TQueueHandle):Boolean;
function QueueNotEmpty(Queue:TQueueHandle):Boolean;

function QueueLock(Queue:TQueueHandle):LongWord; {$IFDEF QUEUE_INLINE}inline;{$ENDIF}
function QueueUnlock(Queue:TQueueHandle):LongWord; {$IFDEF QUEUE_INLINE}inline;{$ENDIF}

{==============================================================================}
{Thread Functions}
function ThreadCreate(StartProc:TThreadStart;StackSize,Priority:LongWord;Name:PChar;Parameter:Pointer):TThreadHandle; {$IFDEF THREAD_INLINE}inline;{$ENDIF}
function ThreadCreateEx(StartProc:TThreadStart;StackSize,Priority,Affinity,CPU:LongWord;Name:PChar;Parameter:Pointer):TThreadHandle;
function ThreadDestroy(Thread:TThreadHandle):LongWord;

function ThreadGetCurrent:TThreadHandle; {$IFDEF THREAD_INLINE}inline;{$ENDIF}
function ThreadSetCurrent(Thread:TThreadHandle):LongWord; {$IFDEF THREAD_INLINE}inline;{$ENDIF}

function ThreadGetName(Thread:TThreadHandle):String;
function ThreadSetName(Thread:TThreadHandle;const Name:String):LongWord;

function ThreadGetCPU(Thread:TThreadHandle):LongWord;
function ThreadSetCPU(Thread:TThreadHandle;CPU:LongWord):LongWord;

function ThreadGetState(Thread:TThreadHandle):LongWord;

function ThreadGetFlags(Thread:TThreadHandle):LongWord;
function ThreadSetFlags(Thread:TThreadHandle;Flags:LongWord):LongWord;
function ThreadAddFlags(Thread:TThreadHandle;Flags:LongWord):LongWord;
function ThreadRemoveFlags(Thread:TThreadHandle;Flags:LongWord):LongWord;

function ThreadGetLocale(Thread:TThreadHandle):LCID;
function ThreadSetLocale(Thread:TThreadHandle;Locale:LCID):LongWord;

function ThreadGetTimes(Thread:TThreadHandle;var CreateTime,ExitTime,KernelTime:Int64):LongWord;
function ThreadGetSwitchCount(Thread:TThreadHandle;var SwitchCount:Int64):LongWord;

function ThreadGetStackFree:LongWord;
function ThreadGetStackSize(Thread:TThreadHandle):LongWord;
function ThreadGetStackBase(Thread:TThreadHandle):PtrUInt;
function ThreadSetStackBase(Thread:TThreadHandle;StackBase:PtrUInt):LongWord;
function ThreadGetStackPointer(Thread:TThreadHandle):PtrUInt;

function ThreadGetExitCode(Thread:TThreadHandle):LongWord;

function ThreadGetAffinity(Thread:TThreadHandle):LongWord;
function ThreadSetAffinity(Thread:TThreadHandle;Affinity:LongWord):LongWord;

function ThreadGetPriority(Thread:TThreadHandle):LongWord;
function ThreadSetPriority(Thread:TThreadHandle;Priority:LongWord):LongWord;

function ThreadGetLastError:LongWord;
procedure ThreadSetLastError(LastError:LongWord);
function ThreadSetLastErrorEx(LastError:LongWord):LongWord;

function ThreadGetWaitResult:LongWord;
function ThreadGetReceiveResult:LongWord;

function ThreadGetTlsIndex(TlsIndex:LongWord):LongWord;
function ThreadAllocTlsIndex:LongWord; inline;
function ThreadAllocTlsIndexEx(Flags:LongWord):LongWord;
function ThreadReleaseTlsIndex(TlsIndex:LongWord):LongWord;
function ThreadGetTlsValue(TlsIndex:LongWord):Pointer;
function ThreadSetTlsValue(TlsIndex:LongWord;TlsValue:Pointer):LongWord;

function ThreadGetTlsPointer(Thread:TThreadHandle):Pointer;
function ThreadSetTlsPointer(Thread:TThreadHandle;TlsPointer:Pointer):LongWord;

function ThreadReady(Thread:TThreadHandle;Reschedule:Boolean):LongWord;
function ThreadTimeout(Thread:TThreadHandle):LongWord;
function ThreadWake(Thread:TThreadHandle):LongWord;

function ThreadMigrate(Thread:TThreadHandle;CPU:LongWord):LongWord;

procedure ThreadEnd(ExitCode:LongWord);
function ThreadHalt(ExitCode:LongWord):LongWord;
function ThreadTerminate(Thread:TThreadHandle;ExitCode:LongWord):LongWord;

function ThreadYield:LongWord;
function ThreadSleep(Milliseconds:LongWord):LongWord;

function ThreadWait(List:TListHandle;Lock:TSpinHandle;Flags:LongWord):LongWord;
function ThreadWaitEx(List:TListHandle;Lock:TSpinHandle;Flags,Timeout:LongWord):LongWord;       {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function ThreadRelease(List:TListHandle):LongWord;
function ThreadAbandon(List:TListHandle):LongWord;
function ThreadWaitTerminate(Thread:TThreadHandle;Timeout:LongWord):LongWord;                   {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}

//To Do //ThreadWaitMultiple(Handles:PListHandles etc /ThreadMultipleWait //Wait on multiple lists (Max 64 items)
//To Do //ThreadReleaseMultiple(Handles:PListHandles etc /ThreadMultipleRelease
//To Do //ThreadAbandonMultiple(Handles:PListHandles etc /ThreadMultipleAbandon

function ThreadSuspend(Thread:TThreadHandle):LongWord;
function ThreadResume(Thread:TThreadHandle):LongWord;

function ThreadWaitMessage:LongWord;
function ThreadSendMessage(Thread:TThreadHandle;const Message:TMessage):LongWord;
//To Do //ThreadSendMessageEx //Wait for response/space with Timeout ?            {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
//To Do //Could also be ThreadPostMessage/Ex (Send with no response wait or not)
//To Do //ThreadReplyMessage //Reply to a received message to release a thread waiting for a reply
function ThreadReceiveMessage(var Message:TMessage):LongWord;
function ThreadReceiveMessageEx(var Message:TMessage;Timeout:LongWord;Remove:Boolean):LongWord; {Timeout = 0 then no Wait, Timeout = INFINITE then Wait forever}
function ThreadAbandonMessage(Thread:TThreadHandle):LongWord;

function ThreadLock(Thread:TThreadHandle):LongWord;
function ThreadUnlock(Thread:TThreadHandle):LongWord;

{==============================================================================}
{Scheduler Functions}
function SchedulerCheck(CPUID:LongWord):LongWord;
function SchedulerWakeup(CPUID:LongWord):LongWord;
function SchedulerExpire(CPUID:LongWord):LongWord;
function SchedulerSwitch(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
function SchedulerSelect(CPUID:LongWord;Thread:TThreadHandle;Yield:Boolean):TThreadHandle;
function SchedulerReschedule(Yield:Boolean):LongWord;

function SchedulerMigrationEnable:LongWord;
function SchedulerMigrationDisable:LongWord;

function SchedulerPreemptEnable(CPUID:LongWord):LongWord;
function SchedulerPreemptDisable(CPUID:LongWord):LongWord;

function SchedulerAllocationEnable(CPUID:LongWord):LongWord;
function SchedulerAllocationDisable(CPUID:LongWord):LongWord;

{==============================================================================}
{Messageslot Functions}
function MessageslotCreate:TMessageslotHandle; {$IFDEF MESSAGESLOT_INLINE}inline;{$ENDIF}
function MessageslotCreateEx(Maximum:LongWord;Flags:LongWord):TMessageslotHandle;
function MessageslotDestroy(Messageslot:TMessageslotHandle):LongWord;

function MessageslotCount(Messageslot:TMessageslotHandle):LongWord;

function MessageslotSend(Messageslot:TMessageslotHandle;const Message:TMessage):LongWord;
function MessageslotReceive(Messageslot:TMessageslotHandle;var Message:TMessage):LongWord;
function MessageslotReceiveEx(Messageslot:TMessageslotHandle;var Message:TMessage;Timeout:LongWord):LongWord;  {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}

{==============================================================================}
{Mailslot Functions}
function MailslotCreate(Maximum:LongWord):TMailslotHandle;
function MailslotDestroy(Mailslot:TMailslotHandle):LongWord;

function MailslotCount(Mailslot:TMailslotHandle):LongWord;

function MailslotSend(Mailslot:TMailslotHandle;Data:PtrInt):LongWord;
function MailslotSendEx(Mailslot:TMailslotHandle;Data:PtrInt;Timeout:LongWord):LongWord;       {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function MailslotReceive(Mailslot:TMailslotHandle):PtrInt;
function MailslotReceiveEx(Mailslot:TMailslotHandle;Timeout:LongWord):PtrInt;                  {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}

{==============================================================================}
{Buffer Functions}
function BufferCreate(Size,Count:LongWord):TBufferHandle; {$IFDEF BUFFER_INLINE}inline;{$ENDIF}
function BufferCreateEx(Size,Count,Flags:LongWord):TBufferHandle;
function BufferDestroy(Buffer:TBufferHandle):LongWord;

function BufferCount(Buffer:TBufferHandle):LongWord;
function BufferAvailable(Buffer:TBufferHandle):LongWord;

function BufferGet(Buffer:TBufferHandle):Pointer;
function BufferGetEx(Buffer:TBufferHandle;Timeout:LongWord):Pointer;                            {Timeout = 0 then No Wait, Timeout = INFINITE then Wait forever}
function BufferFree(Buffer:Pointer):LongWord;

function BufferIterate(Buffer:TBufferHandle;Previous:Pointer):Pointer;

{==============================================================================}
{Event Functions}
function EventCreate(ManualReset,InitialState:Boolean):TEventHandle; {$IFDEF EVENT_INLINE}inline;{$ENDIF}
function EventCreateEx(Flags:LongWord):TEventHandle;
function EventDestroy(Event:TEventHandle):LongWord;

function EventState(Event:TEventHandle):LongWord;

function EventWait(Event:TEventHandle):LongWord;
function EventWaitEx(Event:TEventHandle;Timeout:LongWord):LongWord;                             {Timeout = 0 then no Wait, Timeout = INFINITE then Wait forever}

function EventSet(Event:TEventHandle):LongWord;
function EventReset(Event:TEventHandle):LongWord;
function EventPulse(Event:TEventHandle):LongWord;

{==============================================================================}
{Timer Functions}
function TimerCreate(Interval:LongWord;Enabled,Reschedule:Boolean;Event:TTimerEvent;Data:Pointer):TTimerHandle; {$IFDEF TIMER_INLINE}inline;{$ENDIF}
function TimerCreateEx(Interval,State,Flags:LongWord;Event:TTimerEvent;Data:Pointer):TTimerHandle;
function TimerDestroy(Timer:TTimerHandle):LongWord;

function TimerEnable(Timer:TTimerHandle):LongWord;
function TimerEnableEx(Timer:TTimerHandle;Interval:LongWord;Event:TTimerEvent;Data:Pointer):LongWord;
function TimerDisable(Timer:TTimerHandle):LongWord;

function TimerDequeue:TTimerHandle;

function TimerFirstKey:Integer;
function TimerInsertKey(Timer:TTimerHandle;Key:Integer):LongWord;
function TimerDeleteKey(Timer:TTimerHandle):LongWord;
function TimerDecrementKey:Integer;

function TimerIsEmpty:Boolean;
function TimerNotEmpty:Boolean;

function TimerCheck:LongWord;
function TimerTrigger:LongWord;

{==============================================================================}
{Worker Functions}
function WorkerSchedule(Interval:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
function WorkerScheduleEx(Interval,Flags:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):TWorkerHandle;
function WorkerCancel(Worker:TWorkerHandle):LongWord;

function WorkerScheduleIRQ(Affinity:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
function WorkerScheduleIRQEx(Affinity,Flags:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord;

function WorkerScheduleFIQ(Affinity:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
function WorkerScheduleFIQEx(Affinity,Flags:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord;

function WorkerIncrease(Count:LongWord):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
function WorkerIncreaseEx(Count:LongWord;Priority:Boolean):LongWord;
function WorkerDecrease(Count:LongWord):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
function WorkerDecreaseEx(Count:LongWord;Priority:Boolean):LongWord;

procedure WorkerTimer(WorkerRequest:PWorkerRequest);

{==============================================================================}
{Tasker Functions}
function TaskerThreadSendMessage(Thread:TThreadHandle;const Message:TMessage):LongWord;
function TaskerMessageslotSend(Messageslot:TMessageslotHandle;const Message:TMessage):LongWord;
function TaskerSemaphoreSignal(Semaphore:TSemaphoreHandle;Count:LongWord):LongWord;
function TaskerCompletionReset(Completion:TCompletionHandle):LongWord;
function TaskerCompletionComplete(Completion:TCompletionHandle;All:Boolean):LongWord;

function TaskerEnqueue(Task:PTaskerTask):LongWord;
function TaskerDequeue:PTaskerTask;

function TaskerCheck:LongWord;
function TaskerTrigger:LongWord;

{==============================================================================}
{RTL Init/Exit Functions}
procedure SysInitProc;
procedure SysExitProc;

function SysInitCompleted:Boolean;

{==============================================================================}
{RTL Process Functions}
function SysGetProcessID:SizeUInt;

{==============================================================================}
{RTL Thread Manager Functions}
{See: \source\rtl\inc\thread.inc and \source\rtl\inc\threadh.inc}
{See: \source\rtl\inc\system.inc and \source\rtl\inc\systemh.inc}
function ThreadMain(Parameter:Pointer):PtrInt;

function SysBeginThread(SignalAction:Pointer;StackSize:PtrUInt;ThreadFunction:TThreadFunc;ThreadParameter:Pointer;CreationFlags:DWORD;var ThreadId:TThreadID):TThreadID;
function SysBeginThreadEx(SignalAction:Pointer;StackSize:PtrUInt;ThreadFunction:TThreadFunc;ThreadParameter:Pointer;CreationFlags:DWORD;Priority,Affinity,CPU:LongWord;Name:PChar;var ThreadId:TThreadID):TThreadID;

procedure SysEndThread(ExitCode:DWORD);

function SysSuspendThread(ThreadHandle:TThreadID):DWORD;
function SysResumeThread(ThreadHandle:TThreadID):DWORD;
function SysKillThread(ThreadHandle:TThreadID):DWORD;
function SysCloseThread(ThreadHandle:TThreadID):DWORD;

procedure SysThreadSwitch;

function SysWaitForThreadTerminate(ThreadHandle:TThreadID;TimeoutMs:LongInt):DWORD;  {0=No Timeout}

function SysThreadSetPriority(ThreadHandle:TThreadID;Priority:LongInt):Boolean;      {-15..+15, 0=Normal}
function SysThreadGetPriority(ThreadHandle:TThreadID):LongInt;
function SysGetCurrentThreadId:TThreadID;
procedure SysSetThreadDebugNameA(ThreadHandle:TThreadID;const ThreadName:AnsiString);
procedure SysSetThreadDebugNameU(ThreadHandle:TThreadID;const ThreadName:UnicodeString);

procedure SysInitCriticalSection(var CriticalSection);
procedure SysDoneCriticalSection(var CriticalSection);
procedure SysEnterCriticalSection(var CriticalSection);
function SysTryEnterCriticalSection(var CriticalSection):LongInt;
procedure SysLeaveCriticalSection(var CriticalSection);

procedure SysInitThreadVar(var Offset:DWORD;Size:DWORD);
function SysRelocateThreadVar(Offset:DWORD):Pointer;
procedure SysAllocateThreadVars;
procedure SysReleaseThreadVars;

function SysBasicEventCreate(EventAttributes:Pointer;AManualReset,InitialState:Boolean;const Name:AnsiString):PEventState;
procedure SysBasicEventDestroy(State:PEventState);
procedure SysBasicEventResetEvent(State:PEventState);
procedure SysBasicEventSetEvent(State:PEventState);
{$if defined(FPC_STABLE) or defined(FPC_FIXES) or defined(FPC_LEGACY)}
function SysBasicEventWaitFor(Timeout:Cardinal;State:PEventState):LongInt;           {0=No Timeout}
{$else}
function SysBasicEventWaitFor(Timeout:Cardinal;State:PEventState;UseComWait:Boolean=False):LongInt;           {0=No Timeout}
{$endif}

function SysRTLEventCreate:PRTLEvent;
procedure SysRTLEventDestroy(AEvent:PRTLEvent);
procedure SysRTLEventSetEvent(AEvent:PRTLEvent);
procedure SysRTLEventResetEvent(AEvent:PRTLEvent);
procedure SysRTLEventWaitFor(AEvent:PRTLEvent);
procedure SysRTLEventWaitForTimeout(AEvent:PRTLEvent;Timeout:LongInt);               {0=No Timeout}

function SysSemaphoreInit:Pointer;
procedure SysSemaphoreDestroy(const Semaphore:Pointer);
procedure SysSemaphorePost(const Semaphore:Pointer);
procedure SysSemaphoreWait(const Semaphore:Pointer);

{==============================================================================}
{Thread Helper Functions}
function SpinGetCount:LongWord;

function MutexGetCount:LongWord;

function CriticalSectionGetCount:LongWord;

function SemaphoreGetCount:LongWord;

function SynchronizerGetCount:LongWord;

function ConditionGetCount:LongWord;

function CompletionGetCount:LongWord;

function ListGetCount:LongWord;

function QueueGetCount:LongWord;

function ThreadGetCount:LongWord;

function ThreadTlsGetCount:LongWord;

function ThreadAllocateStack(StackSize:LongWord):Pointer;
procedure ThreadReleaseStack(StackBase:Pointer;StackSize:LongWord);
function ThreadSetupStack(StackBase:Pointer;StartProc:TThreadStart;ReturnProc:TThreadEnd;Parameter:Pointer):Pointer;

function ThreadSnapshotCreate:PThreadSnapshot;
function ThreadSnapshotDestroy(ASnapshot:PThreadSnapshot):LongWord;

function MessageslotGetCount:LongWord;

function MailslotGetCount:LongWord;

function BufferGetCount:LongWord;

function EventGetCount:LongWord;

function TimerGetCount:LongWord;

function WorkerGetCount:LongWord;
function WorkerGetPriorityCount:LongWord;

function TaskerGetCount:LongWord;

function ListTypeToString(ListType:LongWord):String;
function QueueTypeToString(QueueType:LongWord):String;
function ThreadTypeToString(ThreadType:LongWord):String;
function ThreadStateToString(ThreadState:LongWord):String;
function ThreadPriorityToString(ThreadPriority:LongWord):String;

procedure ThreadLog(Level:LongWord;const AText:String);
procedure ThreadLogInfo(const AText:String); inline;
procedure ThreadLogWarn(const AText:String); inline;
procedure ThreadLogError(const AText:String); inline;
procedure ThreadLogDebug(const AText:String); inline;

{==============================================================================}
{Scheduler Helper Functions}
function SchedulerGetListFlags(ListType:LongWord):LongWord;
function SchedulerGetQueueFlags(QueueType:LongWord):LongWord;
function SchedulerGetQueueHandle(CPUID:LongWord;QueueType:LongWord):TQueueHandle;
function SchedulerGetQueueHandleEx(CPUID:LongWord;Priority:LongWord):TQueueHandle;
function SchedulerGetThreadCount(CPUID:LongWord):LongWord;
function SchedulerGetThreadQuantum(CPUID:LongWord):LongWord;
function SchedulerGetThreadHandle(CPUID:LongWord;ThreadType:LongWord):TThreadHandle;
function SchedulerGetPriorityMask(CPUID:LongWord):LongWord;
function SchedulerGetPriorityQuantum(Priority:LongWord):LongWord;
function SchedulerSetPriorityQuantum(Priority,Quantum:LongWord):LongWord;
function SchedulerGetMigrationQuantum:LongWord;
function SchedulerGetStarvationQuantum(CPUID:LongWord):LongWord;

function SchedulerGetThreadNext:LongWord;
function SchedulerGetThreadMigration:LongWord;

function SchedulerGetThreadPreempt(CPUID:LongWord):LongWord;
function SchedulerGetThreadAllocation(CPUID:LongWord):LongWord;

function SchedulerMigrationToString(Migration:LongWord):String;
function SchedulerPreemptToString(Preempt:LongWord):String;
function SchedulerAllocationToString(Allocation:LongWord):String;

{==============================================================================}
{Timer Helper Functions}
function TimerGetListFlags:LongWord;
function TimerGetMessageslotFlags:LongWord;

{==============================================================================}
{Worker Helper Functions}
function WorkerGetMessageslotFlags:LongWord;

{==============================================================================}
{Tasker Helper Functions}

{==============================================================================}
const
 {RTL Thread Manager Structure}
 MyThreadManager: TThreadManager = (
  InitManager:nil;
  DoneManager:nil;
  BeginThread:@SysBeginThread;
  EndThread:@SysEndThread;
  SuspendThread:@SysSuspendThread;
  ResumeThread:@SysResumeThread;
  KillThread:@SysKillThread;
  CloseThread:@SysCloseThread;
  ThreadSwitch:@SysThreadSwitch;
  WaitForThreadTerminate:@SysWaitForThreadTerminate;
  ThreadSetPriority:@SysThreadSetPriority;
  ThreadGetPriority:@SysThreadGetPriority;
  GetCurrentThreadId:@SysGetCurrentThreadId;
  {$IFNDEF FPC_LEGACY}
  SetThreadDebugNameA:@SysSetThreadDebugNameA;
  SetThreadDebugNameU:@SysSetThreadDebugNameU;
  {$ENDIF}
  InitCriticalSection:@SysInitCriticalSection;
  DoneCriticalSection:@SysDoneCriticalSection;
  EnterCriticalSection:@SysEnterCriticalSection;
  TryEnterCriticalSection:@SysTryEnterCriticalSection;
  LeaveCriticalSection:@SysLeaveCriticalSection;
  InitThreadVar:@SysInitThreadVar;
  RelocateThreadVar:@SysRelocateThreadVar;
  AllocateThreadVars:@SysAllocateThreadVars;
  ReleaseThreadVars:@SysReleaseThreadVars;
  BasicEventCreate:@SysBasicEventCreate;
  BasicEventDestroy:@SysBasicEventDestroy;
  BasicEventResetEvent:@SysBasicEventResetEvent;
  BasicEventSetEvent:@SysBasicEventSetEvent;
  BasicEventWaitFor:@SysBasicEventWaitFor;
  RTLEventCreate:@SysRTLEventCreate;
  RTLEventDestroy:@SysRTLEventDestroy;
  RTLEventSetEvent:@SysRTLEventSetEvent;
  RTLEventResetEvent:@SysRTLEventResetEvent;
  RTLEventWaitFor:@SysRTLEventWaitFor;
  RTLEventWaitForTimeout:@SysRTLEventWaitForTimeout;
  {$IFDEF FPC_LEGACY}
  SemaphoreInit:@SysSemaphoreInit; {Removed from current FPC RTL}
  SemaphoreDestroy:@SysSemaphoreDestroy; {Removed from current FPC RTL}
  SemaphorePost:@SysSemaphorePost; {Removed from current FPC RTL}
  SemaphoreWait:@SysSemaphoreWait; {Removed from current FPC RTL}
  {$ENDIF}
 );

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Thread specific variables}
 LocksInitialized:Boolean;
 ThreadsInitialized:Boolean;

 PrimaryInitialized:Boolean;
 SchedulerInitialized:Boolean;
 SecondaryInitialized:Boolean;

 InitializationCompleted:array of LongBool;  {True if all initialization has completed (One per CPU)}

 SysLastInitProc:CodePointer = nil;
 SysInitializationCompleted:Boolean;         {True if unit initialization has completed}

 SpinTable:PSpinEntry;
 SpinTableLock:TSpinHandle;
 SpinTableCount:LongWord;

 MutexTable:PMutexEntry;
 MutexTableLock:TSpinHandle;
 MutexTableCount:LongWord;

 CriticalSectionTable:PCriticalSectionEntry;
 CriticalSectionTableLock:TSpinHandle;
 CriticalSectionTableCount:LongWord;

 SemaphoreTable:PSemaphoreEntry;
 SemaphoreTableLock:TSpinHandle;
 SemaphoreTableCount:LongWord;

 SynchronizerTable:PSynchronizerEntry;
 SynchronizerTableLock:TSpinHandle;
 SynchronizerTableCount:LongWord;

 ConditionTable:PConditionEntry;
 ConditionTableLock:TSpinHandle;
 ConditionTableCount:LongWord;

 CompletionTable:PCompletionEntry;
 CompletionTableLock:TSpinHandle;
 CompletionTableCount:LongWord;

 ListTable:PListEntry;
 ListTableLock:TSpinHandle;
 ListTableCount:LongWord;

 QueueTable:PQueueEntry;
 QueueTableLock:TSpinHandle;
 QueueTableCount:LongWord;

 ThreadTable:PThreadEntry;
 ThreadTableLock:TSpinHandle;
 ThreadTableCount:LongWord;
 ThreadTlsTable:array[0..THREAD_TLS_MAXIMUM - 1] of LongWord; {Thread Local Storage Indexes (for ThreadAllocTlsIndex/ThreadReleaseTlsIndex)}
 ThreadTlsTableCount:LongWord;
 ThreadTerminationTimer:TTimerHandle;

 MessageslotTable:PMessageslotEntry;
 MessageslotTableLock:TSpinHandle;
 MessageslotTableCount:LongWord;

 MailslotTable:PMailslotEntry;
 MailslotTableLock:TSpinHandle;
 MailslotTableCount:LongWord;

 BufferTable:PBufferEntry;
 BufferTableLock:TSpinHandle;
 BufferTableCount:LongWord;

 EventTable:PEventEntry;
 EventTableLock:TSpinHandle;
 EventTableCount:LongWord;

 TimerList:PTimerList;
 TimerTable:PTimerEntry;
 TimerTableLock:TSpinHandle;
 TimerTableCount:LongWord;
 TimerMessageslot:TMessageslotHandle;
 TimerPriorityMessageslot:TMessageslotHandle;

 WorkerThreadLock:TSpinHandle;
 WorkerThreadCount:LongWord;
 WorkerThreadNext:LongWord;
 WorkerMessageslot:TMessageslotHandle;

 WorkerPriorityThreadLock:TSpinHandle;
 WorkerPriorityThreadCount:LongWord;
 WorkerPriorityThreadNext:LongWord;
 WorkerPriorityMessageslot:TMessageslotHandle;

 TaskerList:PTaskerList;

 MainThread:TThreadHandle;

{==============================================================================}
{==============================================================================}
{Spin Forward Declarations}
function SpinLockDefault(SpinEntry:PSpinEntry):LongWord; forward;
function SpinUnlockDefault(SpinEntry:PSpinEntry):LongWord; forward;

function SpinLockIRQDefault(SpinEntry:PSpinEntry):LongWord; forward;
function SpinUnlockIRQDefault(SpinEntry:PSpinEntry):LongWord; forward;

function SpinLockFIQDefault(SpinEntry:PSpinEntry):LongWord; forward;
function SpinUnlockFIQDefault(SpinEntry:PSpinEntry):LongWord; forward;

function SpinLockIRQFIQDefault(SpinEntry:PSpinEntry):LongWord; forward;
function SpinUnlockIRQFIQDefault(SpinEntry:PSpinEntry):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Mutex Forward Declarations}
function MutexLockDefault(MutexEntry:PMutexEntry):LongWord; forward;
function MutexUnlockDefault(MutexEntry:PMutexEntry):LongWord; forward;
function MutexTryLockDefault(MutexEntry:PMutexEntry):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Queue Forward Declarations}
function QueueDequeueEx(Queue:TQueueHandle;Lock,Unlock:Boolean):TThreadHandle; forward;

function QueueFirstKeyEx(Queue:TQueueHandle;Lock,Unlock:Boolean):Integer; forward;
function QueueLastKeyEx(Queue:TQueueHandle;Lock,Unlock:Boolean):Integer; forward;

{==============================================================================}
{==============================================================================}
{Thread Forward Declarations}
procedure ThreadTimer(Data:Pointer); forward;

{==============================================================================}
{==============================================================================}
{Timer Forward Declarations}
function TimerDequeueEx(Lock,Unlock:Boolean):TTimerHandle; forward;

function TimerFirstKeyEx(Lock,Unlock:Boolean):Integer; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure LocksInit;
{Initialize Locks}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if LocksInitialized then Exit;

 {Check InitializationCompleted}
 if Length(InitializationCompleted) = 0 then
  begin
   {Setup InitializationCompleted (Done here to allow for early init)}
   SetLength(InitializationCompleted,CPUGetCount);
  end;

 {Initialize Spin Table (First entry is always its own lock)}
 if SPIN_SHARED_MEMORY then
  begin
   SpinTable:=AllocSharedMem(SizeOf(TSpinEntry));
  end
 else
  begin
   SpinTable:=AllocMem(SizeOf(TSpinEntry));
  end;
 SpinTableLock:=INVALID_HANDLE_VALUE;
 SpinTableCount:=0;
 if SpinTable <> nil then
  begin
   SpinTable.Signature:=SPIN_SIGNATURE;
   SpinTable.State:=SPIN_STATE_UNLOCKED;
   SpinTableLock:=TSpinHandle(SpinTable);
   SpinTableCount:=1;
  end;

 {Initialize Mutex Table}
 MutexTable:=nil;
 MutexTableLock:=SpinCreate;
 MutexTableCount:=0;

 {Initialize Critical Section Table}
 CriticalSectionTable:=nil;
 CriticalSectionTableLock:=SpinCreate;
 CriticalSectionTableCount:=0;

 {Initialize Semaphore Table}
 SemaphoreTable:=nil;
 SemaphoreTableLock:=SpinCreate;
 SemaphoreTableCount:=0;

 {Initialize Synchronizer Table}
 SynchronizerTable:=nil;
 SynchronizerTableLock:=SpinCreate;
 SynchronizerTableCount:=0;

 {Initialize Condition Table}
 ConditionTable:=nil;
 ConditionTableLock:=SpinCreate;
 ConditionTableCount:=0;

 {Initialize Completion Table}
 CompletionTable:=nil;
 CompletionTableLock:=SpinCreate;
 CompletionTableCount:=0;

 {Initialize List Table}
 ListTable:=nil;
 ListTableLock:=SpinCreate;
 ListTableCount:=0;

 {Initialize Queue Table}
 QueueTable:=nil;
 QueueTableLock:=SpinCreate;
 QueueTableCount:=0;

 LocksInitialized:=True;
end;

{==============================================================================}

procedure ThreadsInit;
{Initialize Threading}

{Note: Called only during system startup}
var
 Count:LongWord;
 Thread:TThreadHandle;
begin
 {}
 {Check Initialized}
 if ThreadsInitialized then Exit;

 {Initialize Logging}
 THREAD_LOG_ENABLED:=(THREAD_DEFAULT_LOG_LEVEL <> THREAD_LOG_LEVEL_NONE);

 {Initialize SCHEDULER_FIQ_ENABLED}
 if not(FIQ_ENABLED) then SCHEDULER_FIQ_ENABLED:=False;

 {Initialize SCHEDULER_SWI_ENABLED}
 if not(SWI_ENABLED) then SCHEDULER_SWI_ENABLED:=False;

 {Initialize SCHEDULER_CPU_COUNT/MASK/BOOT}
 SCHEDULER_CPU_COUNT:=CPUGetCount;
 SCHEDULER_CPU_MASK:=CPUGetMask;
 SCHEDULER_CPU_BOOT:=CPUGetBoot;

 {Setup BOOT_STACK_BASE/BOOT_THREAD_HANDLE}
 SetLength(BOOT_STACK_BASE,SCHEDULER_CPU_COUNT);
 SetLength(BOOT_THREAD_HANDLE,SCHEDULER_CPU_COUNT);

 {Setup IDLE_THREAD_HANDLE}
 SetLength(IDLE_THREAD_HANDLE,SCHEDULER_CPU_COUNT);

 {Setup IRQ_STACK_BASE/IRQ_THREAD_HANDLE}
 SetLength(IRQ_STACK_BASE,SCHEDULER_CPU_COUNT);
 SetLength(IRQ_THREAD_HANDLE,SCHEDULER_CPU_COUNT);

 {Setup FIQ_STACK_BASE/FIQ_THREAD_HANDLE}
 SetLength(FIQ_STACK_BASE,SCHEDULER_CPU_COUNT);
 SetLength(FIQ_THREAD_HANDLE,SCHEDULER_CPU_COUNT);

 {Setup SWI_STACK_BASE/SWI_THREAD_HANDLE}
 SetLength(SWI_STACK_BASE,SCHEDULER_CPU_COUNT);
 SetLength(SWI_THREAD_HANDLE,SCHEDULER_CPU_COUNT);

 {Setup ABORT_STACK_BASE}
 SetLength(ABORT_STACK_BASE,SCHEDULER_CPU_COUNT);

 {Setup UNDEFINED_STACK_BASE}
 SetLength(UNDEFINED_STACK_BASE,SCHEDULER_CPU_COUNT);

 {Check InitializationCompleted}
 if Length(InitializationCompleted) = 0 then
  begin
   {Setup InitializationCompleted}
   SetLength(InitializationCompleted,SCHEDULER_CPU_COUNT);
  end;

 {Initialize Primary CPU}
 PrimaryInit;

 {Setup THREAD_NAME_DEFAULT}
 THREAD_NAME_DEFAULT:=RTL_THREAD_NAME;

 {Setup TIMER_THREAD_COUNT/TIMER_PRIORITY_THREAD_COUNT}
 if TIMER_THREAD_COUNT < 1 then TIMER_THREAD_COUNT:=1;
 if TIMER_PRIORITY_THREAD_COUNT < 1 then TIMER_PRIORITY_THREAD_COUNT:=1;

 {Setup WORKER_THREAD_COUNT/WORKER_PRIORITY_THREAD_COUNT}
 if WORKER_THREAD_COUNT < 1 then WORKER_THREAD_COUNT:=1;
 if WORKER_PRIORITY_THREAD_COUNT < 1 then WORKER_PRIORITY_THREAD_COUNT:=1;

 {Setup System CPUCount}
 SystemCPUCount:=SCHEDULER_CPU_COUNT;

 {Setup System InitProc}
 SysLastInitProc:=InitProc;
 InitProc:=@SysInitProc;

 {Setup System ExitProc}
 AddExitProc(SysExitProc);

 {Setup System Handlers}
 SysGetProcessIDHandler:=SysGetProcessID;

 {Setup SysUtils Handlers}
 {Thread Functions}
 SysUtilsSleepHandler:=ThreadSleep;
 {Misc Functions}
 SysUtilsGetLastErrorHandler:=ThreadGetLastError;
 {Locale Functions}
 SysUtilsSysErrorMessageHandler:=SysErrorToString;

 {Setup Global Handlers}
 GetLastErrorHandler:=ThreadGetLastError;
 SetLastErrorHandler:=ThreadSetLastError;

 {Setup Platform Handlers}
 HaltThreadHandler:=ThreadHalt;

 {Setup Spin Default Handlers}
 if not Assigned(SpinLockHandler) then SpinLockHandler:=SpinLockDefault;
 if not Assigned(SpinUnlockHandler) then SpinUnlockHandler:=SpinUnlockDefault;

 if not Assigned(SpinLockIRQHandler) then SpinLockIRQHandler:=SpinLockIRQDefault;
 if not Assigned(SpinUnlockIRQHandler) then SpinUnlockIRQHandler:=SpinUnlockIRQDefault;

 if not Assigned(SpinLockFIQHandler) then SpinLockFIQHandler:=SpinLockFIQDefault;
 if not Assigned(SpinUnlockFIQHandler) then SpinUnlockFIQHandler:=SpinUnlockFIQDefault;

 if not Assigned(SpinLockIRQFIQHandler) then SpinLockIRQFIQHandler:=SpinLockIRQFIQDefault;
 if not Assigned(SpinUnlockIRQFIQHandler) then SpinUnlockIRQFIQHandler:=SpinUnlockIRQFIQDefault;

 {Setup Mutex Default Handlers}
 if not Assigned(MutexLockHandler) then MutexLockHandler:=MutexLockDefault;
 if not Assigned(MutexUnlockHandler) then MutexUnlockHandler:=MutexUnlockDefault;
 if not Assigned(MutexTryLockHandler) then MutexTryLockHandler:=MutexTryLockDefault;

 {Initialize Locale Support}
 LocaleInit;

 {Initialize Unicode Support}
 UnicodeInit;

 {Initialize Locking Primitives}
 LocksInit;

 {Initialize Thread Table (First entry is always the IRQ or FIQ thread on the boot CPU)}
 if THREAD_SHARED_MEMORY then
  begin
   ThreadTable:=AllocSharedMem(SizeOf(TThreadEntry));
  end
 else
  begin
   ThreadTable:=AllocMem(SizeOf(TThreadEntry));
  end;
 ThreadTableLock:=SpinCreate;
 ThreadTableCount:=0;
 ThreadTlsTableCount:=0;
 if ThreadTable <> nil then
  begin
   if SCHEDULER_FIQ_ENABLED then
    begin
     {Setup FIQ Thread}
     {Thread Properties}
     ThreadTable.Signature:=THREAD_SIGNATURE;
     ThreadTable.State:=THREAD_STATE_RUNNING;
     ThreadTable.Flags:=THREAD_FLAG_NONE;
     ThreadTable.Priority:=THREAD_PRIORITY_NORMAL;       {Thread switches itself to THREAD_PRIORITY_NONE after initialization}
     ThreadTable.Affinity:=(1 shl SCHEDULER_CPU_BOOT);   {Must always run on the Boot CPU}
     ThreadTable.StackBase:=Pointer(INITIAL_STACK_BASE); {Assign the Initial Stack to the Initial Thread (This is the stack we are currently running on)}
     ThreadTable.StackSize:=INITIAL_STACK_SIZE;          {Use the Initial Stack size not the FIQ Stack size}
     StrLCopy(ThreadTable.Name,PChar(FIQ_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT)),THREAD_NAME_LENGTH - 1);
     ThreadTable.Lock:=SpinCreate;
     ThreadTable.Parent:=INVALID_HANDLE_VALUE;
     ThreadTable.Messages.Maximum:=THREAD_MESSAGES_MAXIMUM;
     ThreadTable.Messages.List:=AllocMem(SizeOf(TMessage) * ThreadTable.Messages.Maximum);
     ThreadTable.TlsPointer:=AllocMem(INITIAL_TLS_SIZE); {Allocate a default static value for the Initial Thread}
     ThreadTable.ExitCode:=ERROR_SUCCESS;
     ThreadTable.LastError:=ERROR_SUCCESS;
     ThreadTable.Locale:=LOCALE_DEFAULT;
     {Internal Properties}
     ThreadTable.CurrentCPU:=SCHEDULER_CPU_BOOT;         {Must always run on the Boot CPU}
     ThreadTable.StackPointer:=nil;                      {Set on first context switch}
     ThreadTable.TargetCPU:=SCHEDULER_CPU_BOOT;          {Must always run on the Boot CPU}
     ThreadTable.TargetPriority:=THREAD_PRIORITY_NORMAL;
     ThreadTable.List:=ListCreateEx(LIST_TYPE_WAIT_THREAD,SchedulerGetListFlags(LIST_TYPE_WAIT_THREAD));  {INVALID_HANDLE_VALUE;} {Preallocated on threads to prevent IRQ/FIQ deadlocks in ListCreate}
     ThreadTable.WaitList:=INVALID_HANDLE_VALUE;
     ThreadTable.WaitLists:=nil;
     ThreadTable.WaitResult:=ERROR_SUCCESS;
     ThreadTable.ReceiveResult:=ERROR_SUCCESS;
     ThreadTable.ScheduleQueue:=INVALID_HANDLE_VALUE;
     ThreadTable.ListElement.Thread:=TThreadHandle(ThreadTable);
     ThreadTable.QueueElement.Thread:=TThreadHandle(ThreadTable);
     {Statistics Properties}
     ThreadTable.CreateTime:=ClockGetTime;
     ThreadTable.ExitTime:=TIME_TICKS_TO_1899;
     ThreadTable.KernelTime:=TIME_TICKS_TO_1899;
     ThreadTable.SwitchCount:=0;

     {Set Boot Thread}
     BOOT_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=TThreadHandle(ThreadTable);

     {Set Thread Handle}
     FIQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=TThreadHandle(ThreadTable);

     {Set Current Thread}
     ThreadSetCurrent(FIQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT]);

     {Increment Thread Count}
     Inc(ThreadTableCount);
     end
   else
    begin
     {Setup IRQ Thread}
     {Thread Properties}
     ThreadTable.Signature:=THREAD_SIGNATURE;
     ThreadTable.State:=THREAD_STATE_RUNNING;
     ThreadTable.Flags:=THREAD_FLAG_NONE;
     ThreadTable.Priority:=THREAD_PRIORITY_NORMAL;       {Thread switches itself to THREAD_PRIORITY_NONE after initialization}
     ThreadTable.Affinity:=(1 shl SCHEDULER_CPU_BOOT);   {Must always run on the Boot CPU}
     ThreadTable.StackBase:=Pointer(INITIAL_STACK_BASE); {Assign the Initial Stack to the Initial Thread (This is the stack we are currently running on)}
     ThreadTable.StackSize:=INITIAL_STACK_SIZE;          {Use the Initial Stack size not the IRQ Stack size}
     StrLCopy(ThreadTable.Name,PChar(IRQ_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT)),THREAD_NAME_LENGTH - 1);
     ThreadTable.Lock:=SpinCreate;
     ThreadTable.Parent:=INVALID_HANDLE_VALUE;
     ThreadTable.Messages.Maximum:=THREAD_MESSAGES_MAXIMUM;
     ThreadTable.Messages.List:=AllocMem(SizeOf(TMessage) * ThreadTable.Messages.Maximum);
     ThreadTable.TlsPointer:=AllocMem(INITIAL_TLS_SIZE); {Allocate a default static value for the Initial Thread}
     ThreadTable.ExitCode:=ERROR_SUCCESS;
     ThreadTable.LastError:=ERROR_SUCCESS;
     ThreadTable.Locale:=LOCALE_DEFAULT;
     {Internal Properties}
     ThreadTable.CurrentCPU:=SCHEDULER_CPU_BOOT;         {Must always run on the Boot CPU}
     ThreadTable.StackPointer:=nil;                      {Set on first context switch}
     ThreadTable.TargetCPU:=SCHEDULER_CPU_BOOT;          {Must always run on the Boot CPU}
     ThreadTable.TargetPriority:=THREAD_PRIORITY_NORMAL;
     ThreadTable.List:=ListCreateEx(LIST_TYPE_WAIT_THREAD,SchedulerGetListFlags(LIST_TYPE_WAIT_THREAD));  {INVALID_HANDLE_VALUE;} {Preallocated on threads to prevent IRQ/FIQ deadlocks in ListCreate}
     ThreadTable.WaitList:=INVALID_HANDLE_VALUE;
     ThreadTable.WaitLists:=nil;
     ThreadTable.WaitResult:=ERROR_SUCCESS;
     ThreadTable.ReceiveResult:=ERROR_SUCCESS;
     ThreadTable.ScheduleQueue:=INVALID_HANDLE_VALUE;
     ThreadTable.ListElement.Thread:=TThreadHandle(ThreadTable);
     ThreadTable.QueueElement.Thread:=TThreadHandle(ThreadTable);
     {Statistics Properties}
     ThreadTable.CreateTime:=ClockGetTime;
     ThreadTable.ExitTime:=TIME_TICKS_TO_1899;
     ThreadTable.KernelTime:=TIME_TICKS_TO_1899;
     ThreadTable.SwitchCount:=0;

     {Set Boot Thread}
     BOOT_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=TThreadHandle(ThreadTable);

     {Set Thread Handle}
     IRQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=TThreadHandle(ThreadTable);

     {Set Current Thread}
     ThreadSetCurrent(IRQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT]);

     {Increment Thread Count}
     Inc(ThreadTableCount);
    end;
  end;

 {Initialize Messageslot Table}
 MessageslotTable:=nil;
 MessageslotTableLock:=SpinCreate;
 MessageslotTableCount:=0;

 {Initialize Mailslot Table}
 MailslotTable:=nil;
 MailslotTableLock:=SpinCreate;
 MailslotTableCount:=0;

 {Initialize Buffer Table}
 BufferTable:=nil;
 BufferTableLock:=SpinCreate;
 BufferTableCount:=0;

 {Initialize Event Table}
 EventTable:=nil;
 EventTableLock:=SpinCreate;
 EventTableCount:=0;

 {Initialize Timer Table}
 TimerList:=nil;
 TimerTable:=nil;
 TimerTableLock:=SpinCreate;
 TimerTableCount:=0;

 {Initialize Tasker List}
 TaskerList:=nil;

 {Initialize Heap Lock}
 {$IF DEFINED(HEAP_LOCK_IRQ) or DEFINED(HEAP_LOCK_FIQ) or DEFINED(HEAP_LOCK_IRQFIQ)}
 HeapLock.Lock:=SpinCreate;
 {$ELSE}
 if HEAP_LOCK_SPIN then HeapLock.Lock:=SpinCreate else HeapLock.Lock:=MutexCreate;
 {$ENDIF}
 HeapLock.IRQLock:=SpinCreate;
 HeapLock.FIQLock:=SpinCreate;
 {$IFDEF HEAP_LOCK_IRQFIQ}
 if FIQ_ENABLED then HeapLock.AcquireLock:=SpinLockIRQFIQ else HeapLock.AcquireLock:=SpinLockIRQ;
 if FIQ_ENABLED then HeapLock.ReleaseLock:=SpinUnlockIRQFIQ else HeapLock.ReleaseLock:=SpinUnlockIRQ;
 {$ELSE}
  {$IFDEF HEAP_LOCK_FIQ}
  if FIQ_ENABLED then HeapLock.AcquireLock:=SpinLockFIQ else HeapLock.AcquireLock:=SpinLockIRQ;
  if FIQ_ENABLED then HeapLock.ReleaseLock:=SpinUnlockFIQ else HeapLock.ReleaseLock:=SpinUnlockIRQ;
  {$ELSE}
   {$IFDEF HEAP_LOCK_IRQ}
   HeapLock.AcquireLock:=SpinLockIRQ;
   HeapLock.ReleaseLock:=SpinUnlockIRQ;
   {$ELSE}
   if HEAP_LOCK_SPIN then HeapLock.AcquireLock:=SpinLock else HeapLock.AcquireLock:=MutexLock;
   if HEAP_LOCK_SPIN then HeapLock.ReleaseLock:=SpinUnlock else HeapLock.ReleaseLock:=MutexUnlock;
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
 HeapLock.AcquireIRQLock:=SpinLockIRQ;
 HeapLock.ReleaseIRQLock:=SpinUnlockIRQ;
 if FIQ_ENABLED then HeapLock.AcquireFIQLock:=SpinLockIRQFIQ else HeapLock.AcquireFIQLock:=SpinLockIRQ;
 if FIQ_ENABLED then HeapLock.ReleaseFIQLock:=SpinUnlockIRQFIQ else HeapLock.ReleaseFIQLock:=SpinUnlockIRQ;
 RegisterHeapLock(HeapLock);

 {Initialize Clock Lock}
 ClockLock.Lock:=SpinCreate;
 ClockLock.AcquireLock:=SpinLock;
 ClockLock.ReleaseLock:=SpinUnlock;

 {Initialize Power Lock}
 PowerLock.Lock:=SpinCreate;
 PowerLock.AcquireLock:=SpinLock;
 PowerLock.ReleaseLock:=SpinUnlock;

 {Initialize Mailbox Lock}
 MailboxLock.Lock:=SpinCreate;
 MailboxLock.AcquireLock:=SpinLock;
 MailboxLock.ReleaseLock:=SpinUnlock;

 {Initialize Shutdown Lock}
 ShutdownLock.Lock:=SpinCreate;
 ShutdownLock.AcquireLock:=SpinLock;
 ShutdownLock.ReleaseLock:=SpinUnlock;

 {Initialize Interrupt Lock}
 InterruptLock.Lock:=SpinCreate;
 InterruptLock.AcquireLock:=SpinLockIRQFIQ;
 InterruptLock.ReleaseLock:=SpinUnlockIRQFIQ;

 {Initialize Page Table Lock}
 PageTableLock.Lock:=SpinCreate;
 PageTableLock.AcquireLock:=SpinLockIRQFIQ;
 PageTableLock.ReleaseLock:=SpinUnlockIRQFIQ;

 {Initialize Vector Table Lock}
 VectorTableLock.Lock:=SpinCreate;
 VectorTableLock.AcquireLock:=SpinLockIRQFIQ;
 VectorTableLock.ReleaseLock:=SpinUnlockIRQFIQ;

 {Initialize Handle Name Lock}
 HandleNameLock.Lock:=MutexCreate;
 HandleNameLock.AcquireLock:=MutexLock;
 HandleNameLock.ReleaseLock:=MutexUnlock;

 {Initialize Handle Table Lock}
 HandleTableLock.Lock:=SpinCreate;
 HandleTableLock.AcquireLock:=SpinLock;
 HandleTableLock.ReleaseLock:=SpinUnlock;

 {Initialize Shutdown Table Lock}
 ShutdownTableLock.Lock:=SpinCreate;
 ShutdownTableLock.AcquireLock:=SpinLock;
 ShutdownTableLock.ReleaseLock:=SpinUnlock;

 {Initialize Utility Lock}
 UtilityLock.Lock:=SpinCreate;
 UtilityLock.AcquireLock:=SpinLock;
 UtilityLock.ReleaseLock:=SpinUnlock;

 {Initialize Environment Lock}
 EnvironmentLock.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 EnvironmentLock.AcquireLock:=MutexLock;
 EnvironmentLock.ReleaseLock:=MutexUnlock;

 {Initialize Shutdown Semaphore}
 ShutdownSemaphore.Semaphore:=SemaphoreCreate(0);
 ShutdownSemaphore.WaitSemaphore:=SemaphoreWaitEx;
 ShutdownSemaphore.SignalSemaphore:=SemaphoreSignal;

 {Initialize Shutdown Worker}
 ShutdownWorker.ScheduleWorker:=WorkerSchedule;

 {Initialize CodePage Lock}
 CodePageLock.Lock:=MutexCreate;
 CodePageLock.AcquireLock:=MutexLock;
 CodePageLock.ReleaseLock:=MutexUnlock;

 {Initialize Thread Manager}
 SetThreadManager(MyThreadManager);

 {Initialize Thread Variables}
 InitThreadVars(@SysRelocateThreadVar);
 IsMultiThread:=True;

 {Setup Initial Thread Tls Memory}
 {ThreadTable.TlsPointer:=AllocMem(ThreadVarBlockSize);} {Must be done statically above for the Initial Thread}
 if ThreadVarBlockSize > INITIAL_TLS_SIZE then
  begin
   {$IFDEF THREAD_DEBUG}
   if THREAD_LOG_ENABLED then ThreadLogDebug('TLS block size exceeds initial allocation, System Halted');
   {$ENDIF}

   Halt;
  end;

 {Initialize Thread}
 InitThread(INITIAL_STACK_SIZE);

 {Initialize Standard Text IO (Input/Output/ErrOutput/StdOut/StdErr)}
 {Note: Normally done by InitThread}
 TextIOOpen(Input,TextIOWriteChar,TextIOReadChar,fmInput,nil);
 TextIOOpen(Output,TextIOWriteChar,TextIOReadChar,fmOutput,nil);
 TextIOOpen(ErrOutput,TextIOWriteChar,TextIOReadChar,fmOutput,nil);
 TextIOOpen(StdOut,TextIOWriteChar,TextIOReadChar,fmOutput,nil);
 TextIOOpen(StdErr,TextIOWriteChar,TextIOReadChar,fmOutput,nil);

 {Initialize InOutRes}
 {Note: Normally done by InitThread}
 InOutRes:=0;

 {Initialize Stack Checking}
 {Note: Normally done by InitThread}
 {StackLength:=CheckInitialStkLen(ThreadInfo.StackLength);}
 {StackBottom:=Sptr - StackLength;}

 {Initialize Exception Support}
 SysUtilsInitExceptions;

 {Initialize Unhandled Exceptions}
 ExceptProc:=@UnhandledException;

 {Initialize Scheduler}
 SchedulerInit;

 if SCHEDULER_FIQ_ENABLED then
  begin
   {Create IRQ Thread (FIQ Thread is the Initial Thread)}
   IRQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=SysBeginThreadEx(nil,IRQ_STACK_SIZE,IRQExecute,nil,0,THREAD_PRIORITY_NORMAL,(1 shl SCHEDULER_CPU_BOOT),SCHEDULER_CPU_BOOT,PChar(IRQ_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT)),Thread);
   if IRQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT] = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create IRQ thread, System Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup IRQ Thread}
   {Name}
   ThreadSetName(IRQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT],IRQ_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT));
   {Priority (Thread switches itself to THREAD_PRIORITY_NONE after initialization)}
   ThreadSetPriority(IRQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT],THREAD_PRIORITY_NORMAL);
   {Affinity (Must always run on the Boot CPU)}
   ThreadSetAffinity(IRQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT],(1 shl SCHEDULER_CPU_BOOT));
  end
 else
  begin
   {Create FIQ Thread (IRQ Thread is the Initial Thread)}
   if FIQ_ENABLED then
    begin
     FIQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=SysBeginThreadEx(nil,FIQ_STACK_SIZE,FIQExecute,nil,0,THREAD_PRIORITY_NORMAL,(1 shl SCHEDULER_CPU_BOOT),SCHEDULER_CPU_BOOT,PChar(FIQ_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT)),Thread);
     if FIQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT] = INVALID_HANDLE_VALUE then
      begin
       {$IFDEF THREAD_DEBUG}
       if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create FIQ thread, System Halted');
       {$ENDIF}

       Halt;
      end;
     {Setup FIQ Thread}
     {Name}
     ThreadSetName(FIQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT],FIQ_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT));
     {Priority (Thread switches itself to THREAD_PRIORITY_NONE after initialization)}
     ThreadSetPriority(FIQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT],THREAD_PRIORITY_NORMAL);
     {Affinity (Must always run on the Boot CPU)}
     ThreadSetAffinity(FIQ_THREAD_HANDLE[SCHEDULER_CPU_BOOT],(1 shl SCHEDULER_CPU_BOOT));
    end;
  end;

 {Create SWI Thread}
 if SWI_ENABLED then
  begin
   SWI_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=SysBeginThreadEx(nil,SWI_STACK_SIZE,SWIExecute,nil,0,THREAD_PRIORITY_NORMAL,(1 shl SCHEDULER_CPU_BOOT),SCHEDULER_CPU_BOOT,PChar(SWI_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT)),Thread);
   if SWI_THREAD_HANDLE[SCHEDULER_CPU_BOOT] = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create SWI thread, System Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup SWI Thread}
   {Name}
   ThreadSetName(SWI_THREAD_HANDLE[SCHEDULER_CPU_BOOT],SWI_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT));
   {Priority (Thread switches itself to THREAD_PRIORITY_NONE after initialization)}
   ThreadSetPriority(SWI_THREAD_HANDLE[SCHEDULER_CPU_BOOT],THREAD_PRIORITY_NORMAL);
   {Affinity (Must always run on the Boot CPU)}
   ThreadSetAffinity(SWI_THREAD_HANDLE[SCHEDULER_CPU_BOOT],(1 shl SCHEDULER_CPU_BOOT));
  end;

 {Create Idle Thread}
 IDLE_THREAD_HANDLE[SCHEDULER_CPU_BOOT]:=SysBeginThreadEx(nil,IDLE_STACK_SIZE,IdleExecute,nil,0,THREAD_PRIORITY_IDLE,(1 shl SCHEDULER_CPU_BOOT),SCHEDULER_CPU_BOOT,PChar(IDLE_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT)),Thread);
 if IDLE_THREAD_HANDLE[SCHEDULER_CPU_BOOT] = INVALID_HANDLE_VALUE then
  begin
   {$IFDEF THREAD_DEBUG}
   if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create idle thread, System Halted');
   {$ENDIF}

   Halt;
  end;
 {Setup Idle Thread}
 {Name}
 ThreadSetName(IDLE_THREAD_HANDLE[SCHEDULER_CPU_BOOT],IDLE_THREAD_NAME + IntToStr(SCHEDULER_CPU_BOOT));
 {Priority}
 ThreadSetPriority(IDLE_THREAD_HANDLE[SCHEDULER_CPU_BOOT],THREAD_PRIORITY_IDLE);
 {Affinity (Must always run on the Boot CPU)}
 ThreadSetAffinity(IDLE_THREAD_HANDLE[SCHEDULER_CPU_BOOT],(1 shl SCHEDULER_CPU_BOOT));

 {Create Main Thread}
 MainThread:=SysBeginThreadEx(nil,THREAD_STACK_DEFAULT_SIZE,MainExecute,nil,0,THREAD_PRIORITY_NORMAL,SCHEDULER_CPU_MASK,SCHEDULER_CPU_BOOT,PChar(MAIN_THREAD_NAME),Thread);
 if MainThread = INVALID_HANDLE_VALUE then
  begin
   {$IFDEF THREAD_DEBUG}
   if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create main thread, System Halted');
   {$ENDIF}

   Halt;
  end;
 {Setup Main Thread}
 ThreadSetName(MainThread,MAIN_THREAD_NAME);

 {Create Timer List}
 TimerList:=AllocMem(SizeOf(TTimerList));
 if TimerList <> nil then
  begin
   TimerList.Count:=0;
   TimerList.Flags:=TimerGetListFlags;
   TimerList.Lock:=SpinCreate;
   TimerList.First:=nil;
   TimerList.Last:=nil;
  end;

 {Create Timer Messageslot}
 TimerMessageslot:=MessageslotCreateEx(TIMER_MESSAGESLOT_MAXIMUM,TimerGetMessageslotFlags);

 {Create Timer Threads}
 for Count:=0 to TIMER_THREAD_COUNT - 1 do
  begin
   {Create Timer Thread}
   Thread:=BeginThread(TimerExecute,nil,Thread,THREAD_STACK_DEFAULT_SIZE);
   if Thread = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create timer thread, System Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup Timer Thread}
   {Name}
   ThreadSetName(Thread,TIMER_THREAD_NAME + IntToStr(Count));
   {Priority}
   ThreadSetPriority(Thread,TIMER_THREAD_PRIORITY);
  end;

 {Create Timer Priority Messageslot}
 TimerPriorityMessageslot:=MessageslotCreateEx(TIMER_MESSAGESLOT_MAXIMUM,TimerGetMessageslotFlags);

 {Create Timer Priority Threads}
 for Count:=0 to TIMER_PRIORITY_THREAD_COUNT - 1 do
  begin
   {Create Timer Priority Thread}
   Thread:=BeginThread(TimerPriorityExecute,nil,Thread,THREAD_STACK_DEFAULT_SIZE);
   if Thread = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create timer priority thread, System Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup Timer Priority Thread}
   {Name}
   ThreadSetName(Thread,TIMER_PRIORITY_THREAD_NAME + IntToStr(Count));
   {Priority}
   ThreadSetPriority(Thread,TIMER_PRIORITY_THREAD_PRIORITY);
  end;

 {Create Worker Lock}
 WorkerThreadLock:=SpinCreate;
 WorkerThreadCount:=0;
 WorkerThreadNext:=0;

 {Create Worker Messageslot}
 WorkerMessageslot:=MessageslotCreateEx(WORKER_MESSAGESLOT_MAXIMUM,WorkerGetMessageslotFlags);

 {Create Worker Threads}
 for Count:=0 to WORKER_THREAD_COUNT - 1 do
  begin
   {Create Worker Thread}
   Thread:=BeginThread(WorkerExecute,nil,Thread,THREAD_STACK_DEFAULT_SIZE);
   if Thread = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create worker thread, System Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup Worker Thread}
   {Name}
   ThreadSetName(Thread,WORKER_THREAD_NAME + IntToStr(WorkerThreadNext));
   {Priority}
   ThreadSetPriority(Thread,WORKER_THREAD_PRIORITY);

   Inc(WorkerThreadNext);
  end;

 {Create Worker Priority Lock}
 WorkerPriorityThreadLock:=SpinCreate;
 WorkerPriorityThreadCount:=0;
 WorkerPriorityThreadNext:=0;

 {Create Worker Priority Messageslot}
 WorkerPriorityMessageslot:=MessageslotCreateEx(WORKER_MESSAGESLOT_MAXIMUM,WorkerGetMessageslotFlags);

 {Create Worker Priority Threads}
 for Count:=0 to WORKER_PRIORITY_THREAD_COUNT - 1 do
  begin
   {Create Worker Priority Thread}
   Thread:=BeginThread(WorkerPriorityExecute,nil,Thread,THREAD_STACK_DEFAULT_SIZE);
   if Thread = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create worker priority thread, System Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup Worker Priority Thread}
   {Name}
   ThreadSetName(Thread,WORKER_PRIORITY_THREAD_NAME + IntToStr(WorkerPriorityThreadNext));
   {Priority}
   ThreadSetPriority(Thread,WORKER_PRIORITY_THREAD_PRIORITY);

   Inc(WorkerPriorityThreadNext);
  end;

 {Create Tasker List}
 TaskerList:=AllocMem(SizeOf(TTaskerList));
 if TaskerList <> nil then
  begin
   TaskerList.Count:=0;
   TaskerList.Lock:=SpinCreate;
   TaskerList.First:=nil;
   TaskerList.Last:=nil;
  end;

 {Create Termination Timer}
 ThreadTerminationTimer:=TimerCreateEx(SCHEDULER_TERMINATION_INITIAL,TIMER_STATE_ENABLED,TIMER_FLAG_RESCHEDULE or TIMER_FLAG_WORKER,TTimerEvent(ThreadTimer),nil); {Rescheduled Automatically}

 {Calibrate Idle Thread}
 SCHEDULER_IDLE_PER_SECOND:=IdleCalibrate;

 ThreadsInitialized:=True;

 {Synchronization Barrier}
 DataSynchronizationBarrier;

 if SCHEDULER_FIQ_ENABLED then
  begin
   {Call the FIQ Thread Handler}
   FIQExecute(nil);
  end
 else
  begin
   {Call the IRQ Thread Handler}
   IRQExecute(nil);
  end;
end;

{==============================================================================}

procedure PrimaryInit;
{Initialize the primary CPU}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if PrimaryInitialized then Exit;

 {Perform default initialization}
 {Setup Boot Stack}
 BOOT_STACK_BASE[SCHEDULER_CPU_BOOT]:=INITIAL_STACK_BASE;

 {Create IRQ Stack}
 if IRQ_ENABLED and IRQ_STACK_ENABLED then
  begin
   IRQ_STACK_BASE[SCHEDULER_CPU_BOOT]:=PtrUInt(AllocAlignedMem(IRQ_STACK_SIZE,STACK_MIN_ALIGNMENT));
   {Get the top of the IRQ Stack}
   if IRQ_STACK_BASE[SCHEDULER_CPU_BOOT] <> 0 then IRQ_STACK_BASE[SCHEDULER_CPU_BOOT]:=IRQ_STACK_BASE[SCHEDULER_CPU_BOOT] + (IRQ_STACK_SIZE - STACK_MIN_ALIGNMENT);
  end;

 {Create FIQ Stack}
 if FIQ_ENABLED and FIQ_STACK_ENABLED then
  begin
   FIQ_STACK_BASE[SCHEDULER_CPU_BOOT]:=PtrUInt(AllocAlignedMem(FIQ_STACK_SIZE,STACK_MIN_ALIGNMENT));
   {Get the top of the FIQ Stack}
   if FIQ_STACK_BASE[SCHEDULER_CPU_BOOT] <> 0 then FIQ_STACK_BASE[SCHEDULER_CPU_BOOT]:=FIQ_STACK_BASE[SCHEDULER_CPU_BOOT] + (FIQ_STACK_SIZE - STACK_MIN_ALIGNMENT);
  end;

 {Create SWI Stack}
 if SWI_ENABLED and SWI_STACK_ENABLED then
  begin
   SWI_STACK_BASE[SCHEDULER_CPU_BOOT]:=PtrUInt(AllocAlignedMem(SWI_STACK_SIZE,STACK_MIN_ALIGNMENT));
   {Get the top of the SWI Stack}
   if SWI_STACK_BASE[SCHEDULER_CPU_BOOT] <> 0 then SWI_STACK_BASE[SCHEDULER_CPU_BOOT]:=SWI_STACK_BASE[SCHEDULER_CPU_BOOT] + (SWI_STACK_SIZE - STACK_MIN_ALIGNMENT);
  end;

 {Create ABORT Stack}
 if ABORT_ENABLED and ABORT_STACK_ENABLED then
  begin
   ABORT_STACK_BASE[SCHEDULER_CPU_BOOT]:=PtrUInt(AllocAlignedMem(ABORT_STACK_SIZE,STACK_MIN_ALIGNMENT));
   {Get the top of the ABORT Stack}
   if ABORT_STACK_BASE[SCHEDULER_CPU_BOOT] <> 0 then ABORT_STACK_BASE[SCHEDULER_CPU_BOOT]:=ABORT_STACK_BASE[SCHEDULER_CPU_BOOT] + (ABORT_STACK_SIZE - STACK_MIN_ALIGNMENT);
  end;

 {Create UNDEFINED Stack}
 if UNDEFINED_ENABLED and UNDEFINED_STACK_ENABLED then
  begin
   UNDEFINED_STACK_BASE[SCHEDULER_CPU_BOOT]:=PtrUInt(AllocAlignedMem(UNDEFINED_STACK_SIZE,STACK_MIN_ALIGNMENT));
   {Get the top of the UNDEFINED Stack}
   if UNDEFINED_STACK_BASE[SCHEDULER_CPU_BOOT] <> 0 then UNDEFINED_STACK_BASE[SCHEDULER_CPU_BOOT]:=UNDEFINED_STACK_BASE[SCHEDULER_CPU_BOOT] + (UNDEFINED_STACK_SIZE - STACK_MIN_ALIGNMENT);
  end;

 {Check the Handler}
 if Assigned(PrimaryInitHandler) then
  begin
   {Call the Handler}
   PrimaryInitHandler;
  end;

 {Perform default initialization}
 {Nothing}

 PrimaryInitialized:=True;
end;

{==============================================================================}

procedure SchedulerInit;
{Initialize the thread scheduler}

{Note: Called only during system startup}
var
 Count:LongWord;
begin
 {}
 {Check Initialized}
 if SchedulerInitialized then Exit;

 {Perform default initialization}
 {Setup SCHEDULER_PRIORITY_MASK}
 SetLength(SCHEDULER_PRIORITY_MASK,THREAD_PRIORITY_COUNT);

 {Setup SCHEDULER_PRIORITY_QUANTUM}
 SetLength(SCHEDULER_PRIORITY_QUANTUM,THREAD_PRIORITY_COUNT);

 {Setup SchedulerThreadCount}
 SetLength(SchedulerThreadCount,SCHEDULER_CPU_COUNT);

 {Setup SchedulerThreadQuantum}
 SetLength(SchedulerThreadQuantum,SCHEDULER_CPU_COUNT);

 {Setup SchedulerThreadPreempt}
 SetLength(SchedulerThreadPreempt,SCHEDULER_CPU_COUNT);

 {Setup SchedulerThreadAllocation}
 SetLength(SchedulerThreadAllocation,SCHEDULER_CPU_COUNT);

 {Setup SchedulerPriorityMask}
 SetLength(SchedulerPriorityMask,SCHEDULER_CPU_COUNT);

 {$IFNDEF SCHEDULER_YIELD_ALTERNATE}
 {Setup SchedulerYieldCurrent}
 SetLength(SchedulerYieldCurrent,SCHEDULER_CPU_COUNT);
 {$ENDIF}

 {Setup SchedulerStarvationNext}
 SetLength(SchedulerStarvationNext,SCHEDULER_CPU_COUNT);

 {Setup SchedulerStarvationQuantum}
 SetLength(SchedulerStarvationQuantum,SCHEDULER_CPU_COUNT);

 {Setup SchedulerNoneQueue}
 SetLength(SchedulerNoneQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerIdleQueue}
 SetLength(SchedulerIdleQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerLowestQueue}
 SetLength(SchedulerLowestQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerLowerQueue}
 SetLength(SchedulerLowerQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerNormalQueue}
 SetLength(SchedulerNormalQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerHigherQueue}
 SetLength(SchedulerHigherQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerHighestQueue}
 SetLength(SchedulerHighestQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerCriticalQueue}
 SetLength(SchedulerCriticalQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSleepQueue}
 SetLength(SchedulerSleepQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerTimeoutQueue}
 SetLength(SchedulerTimeoutQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerTerminationQueue}
 SetLength(SchedulerTerminationQueue,SCHEDULER_CPU_COUNT);

 {Setup SchedulerLast}
 SetLength(SchedulerLast,SCHEDULER_CPU_COUNT);

 {Setup SchedulerInterrupts}
 SetLength(SchedulerInterrupts,SCHEDULER_CPU_COUNT);

 {$IFDEF SCHEDULER_DEBUG}
 {Setup SchedulerInterruptCounter}
 SetLength(SchedulerInterruptCounter,SCHEDULER_CPU_COUNT);

 {Setup SchedulerInterruptOffset}
 SetLength(SchedulerInterruptOffset,SCHEDULER_CPU_COUNT);

 {Setup SchedulerInterruptMinOffset}
 SetLength(SchedulerInterruptMinOffset,SCHEDULER_CPU_COUNT);

 {Setup SchedulerInterruptMaxOffset}
 SetLength(SchedulerInterruptMaxOffset,SCHEDULER_CPU_COUNT);

 {Setup SchedulerInterruptRollover}
 SetLength(SchedulerInterruptRollover,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectEntry}
 SetLength(SchedulerSelectEntry,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectYield}
 SetLength(SchedulerSelectYield,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectForce}
 SetLength(SchedulerSelectForce,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectNoMask}
 SetLength(SchedulerSelectNoMask,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectNormal}
 SetLength(SchedulerSelectNormal,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectInvalid}
 SetLength(SchedulerSelectInvalid,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectFailure}
 SetLength(SchedulerSelectFailure,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectNoReady}
 SetLength(SchedulerSelectNoReady,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectDefaulted}
 SetLength(SchedulerSelectDefaulted,SCHEDULER_CPU_COUNT);

 {Setup SchedulerStarvationReset}
 SetLength(SchedulerStarvationReset,SCHEDULER_CPU_COUNT);

 {Setup SchedulerStarvationDecrement}
 SetLength(SchedulerStarvationDecrement,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectCPU}
 SetLength(SchedulerSelectCPU,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectPriority}
 SetLength(SchedulerSelectPriority,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSelectAffinity}
 SetLength(SchedulerSelectAffinity,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSwitchEntry}
 SetLength(SchedulerSwitchEntry,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSwitchThread}
 SetLength(SchedulerSwitchThread,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSwitchCounter}
 SetLength(SchedulerSwitchCounter,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSwitchCurrent}
 SetLength(SchedulerSwitchCurrent,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSwitchInvalid}
 SetLength(SchedulerSwitchInvalid,SCHEDULER_CPU_COUNT);

 {Setup SchedulerRescheduleEntry}
 SetLength(SchedulerRescheduleEntry,SCHEDULER_CPU_COUNT);

 {Setup SchedulerRescheduleThread}
 SetLength(SchedulerRescheduleThread,SCHEDULER_CPU_COUNT);

 {Setup SchedulerRescheduleCounter}
 SetLength(SchedulerRescheduleCounter,SCHEDULER_CPU_COUNT);

 {Setup SchedulerRescheduleCurrent}
 SetLength(SchedulerRescheduleCurrent,SCHEDULER_CPU_COUNT);

 {Setup SchedulerRescheduleInvalid}
 SetLength(SchedulerRescheduleInvalid,SCHEDULER_CPU_COUNT);

 {Setup SchedulerTerminationCounter}
 SetLength(SchedulerTerminationCounter,SCHEDULER_CPU_COUNT);

 {Setup SchedulerSecondaryWaitCounter}
 SetLength(SchedulerSecondaryWaitCounter,SCHEDULER_CPU_COUNT);
 {$ENDIF}

 {$IF DEFINED(IRQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 {Setup DispatchInterruptCounter}
 SetLength(DispatchInterruptCounter,SCHEDULER_CPU_COUNT);
 {$ENDIF}
 {$IF DEFINED(FIQ_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 {Setup DispatchFastInterruptCounter}
 SetLength(DispatchFastInterruptCounter,SCHEDULER_CPU_COUNT);
 {$ENDIF}
 {$IF DEFINED(SWI_STATISTICS) or DEFINED(INTERRUPT_DEBUG)}
 {Setup DispatchSystemCallCounter}
 SetLength(DispatchSystemCallCounter,SCHEDULER_CPU_COUNT);
 {$ENDIF}

 {Setup UtilizationLast/Current}
 SetLength(UtilizationLast,SCHEDULER_CPU_COUNT);
 SetLength(UtilizationCurrent,SCHEDULER_CPU_COUNT);

 {Initialize SCHEDULER_THREAD_QUANTUM}
 {Use Default}

 {Initialize SCHEDULER_MIGRATION_QUANTUM}
 {Use Default}

 {Initialize SCHEDULER_STARVATION_QUANTUM}
 {Use Default}

 {Initialize SCHEDULER_TERMINATION_QUANTUM}
 {Use Default}

 {Get Priority Count}
 for Count:=0 to THREAD_PRIORITY_COUNT - 1 do
  begin
   {Initialize SCHEDULER_PRIORITY_MASK}
   SCHEDULER_PRIORITY_MASK[Count]:=SCHEDULER_MASKS[Count];

   {Initialize SCHEDULER_PRIORITY_QUANTUM}
   SCHEDULER_PRIORITY_QUANTUM[Count]:=SCHEDULER_QUANTUMS[Count];
  end;

 {Check the Handler}
 if Assigned(SchedulerInitHandler) then
  begin
   {Call the Handler}
   SchedulerInitHandler;
  end;

 {Perform default initialization}
 {Initialize SchedulerThreadNext}
 SchedulerThreadNext:=SCHEDULER_CPU_BOOT;

 {Initialize SchedulerThreadMigration}
 SchedulerThreadMigration:=SCHEDULER_MIGRATION_ENABLED;
 if SCHEDULER_SECONDARY_DISABLED then SchedulerThreadMigration:=SCHEDULER_MIGRATION_DISABLED;

 {Initialize SchedulerMigrationQuantum}
 SchedulerMigrationQuantum:=SCHEDULER_MIGRATION_QUANTUM;

 {Get CPU Count}
 for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
  begin
   {Initialize SchedulerThreadCount}
   SchedulerThreadCount[Count]:=0;
   {if Count = SCHEDULER_CPU_BOOT then SchedulerThreadCount[Count]:=1;} {Ready threads only}

   {Initialize SchedulerThreadQuantum}
   SchedulerThreadQuantum[Count]:=0;
   if Count = SCHEDULER_CPU_BOOT then SchedulerThreadQuantum[Count]:=SCHEDULER_THREAD_QUANTUM;

   {Initialize SchedulerThreadPreempt}
   SchedulerThreadPreempt[Count]:=SCHEDULER_PREEMPT_ENABLED;

   {Initialize SchedulerThreadAllocation}
   SchedulerThreadAllocation[Count]:=SCHEDULER_ALLOCATION_ENABLED;
   if ((1 shl Count) and SCHEDULER_CPU_RESERVE) <> 0 then SchedulerThreadAllocation[Count]:=SCHEDULER_ALLOCATION_DISABLED;
   if (Count <> SCHEDULER_CPU_BOOT) and (SCHEDULER_SECONDARY_DISABLED) then SchedulerThreadAllocation[Count]:=SCHEDULER_ALLOCATION_DISABLED;

   {Initialize SchedulerPriorityMask}
   SchedulerPriorityMask[Count]:=0;

   {$IFNDEF SCHEDULER_YIELD_ALTERNATE}
   {Initialize SchedulerYieldCurrent}
   SchedulerYieldCurrent[Count]:=False;
   {$ENDIF}

   {Initialize SchedulerStarvationNext}
   SchedulerStarvationNext[Count]:=THREAD_PRIORITY_NORMAL;

   {Initialize SchedulerStarvationQuantum}
   SchedulerStarvationQuantum[Count]:=SCHEDULER_STARVATION_QUANTUM;

   {Initialize SchedulerNoneQueue}
   SchedulerNoneQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_NONE,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_NONE));

   {Initialize SchedulerIdleQueue}
   SchedulerIdleQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_IDLE,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_IDLE));

   {Initialize SchedulerLowestQueue}
   SchedulerLowestQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_LOWEST,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_LOWEST));

   {Initialize SchedulerLowerQueue}
   SchedulerLowerQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_LOWER,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_LOWER));

   {Initialize SchedulerNormalQueue}
   SchedulerNormalQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_NORMAL,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_NORMAL));

   {Initialize SchedulerHigherQueue}
   SchedulerHigherQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_HIGHER,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_HIGHER));

   {Initialize SchedulerHighestQueue}
   SchedulerHighestQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_HIGHEST,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_HIGHEST));

   {Initialize SchedulerCriticalQueue}
   SchedulerCriticalQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_CRITICAL,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_CRITICAL));

   {Initialize SchedulerSleepQueue}
   SchedulerSleepQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_SLEEP,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_SLEEP));

   {Initialize SchedulerTimeoutQueue}
   SchedulerTimeoutQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_TIMEOUT,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_TIMEOUT));

   {Initialize SchedulerTerminationQueue}
   SchedulerTerminationQueue[Count]:=QueueCreateEx(QUEUE_TYPE_SCHEDULE_TERMINATION,SchedulerGetQueueFlags(QUEUE_TYPE_SCHEDULE_TERMINATION));

   {Initialize SchedulerLast}
   SchedulerLast[Count]:=0;

   {Initialize SchedulerInterrupts}
   SchedulerInterrupts[Count]:=0;

   {Initialize UtilizationLast/Current}
   UtilizationLast[Count]:=0;
   UtilizationCurrent[Count]:=SCHEDULER_IDLE_PER_SECOND;
  end;

 SchedulerInitialized:=True;
end;

{==============================================================================}

procedure SchedulerStart(CPUID:LongWord);
{Initialize the thread scheduler for secondary CPUs (Where Applicable)}

{Note: Called only during system startup}
begin
 {}
 if Assigned(SchedulerStartHandler) then
  begin
   SchedulerStartHandler(CPUID);
  end;
end;

{==============================================================================}

procedure SecondaryInit;
{Initialize the secondary CPUs (Where Applicable)}

{Note: Called only during system startup}
var
 Count:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 {Check Initialized}
 if SecondaryInitialized then Exit;

 {Perform default initialization}
 {Nothing}

 {Check the Handler}
 if Assigned(SecondaryInitHandler) then
  begin
   {Call the Handler}
   SecondaryInitHandler;
  end;

 {Perform default initialization}
 {Check CPU Count}
 if SCHEDULER_CPU_COUNT > 1 then
  begin
   {Boot CPUs 0 to SCHEDULER_CPU_COUNT - 1}
   for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
    begin
     {Check Boot CPU}
     if Count <> SCHEDULER_CPU_BOOT then
      begin
       {Create Boot Stack}
       BOOT_STACK_BASE[Count]:=PtrUInt(AllocAlignedMem(BOOT_STACK_SIZE,STACK_MIN_ALIGNMENT));
       {Get the top of the Boot Stack}
       if BOOT_STACK_BASE[Count] <> 0 then BOOT_STACK_BASE[Count]:=BOOT_STACK_BASE[Count] + (BOOT_STACK_SIZE - STACK_MIN_ALIGNMENT);

       {Create IRQ Stack}
       if IRQ_ENABLED and IRQ_STACK_ENABLED then
        begin
         IRQ_STACK_BASE[Count]:=PtrUInt(AllocAlignedMem(IRQ_STACK_SIZE,STACK_MIN_ALIGNMENT));
         {Get the top of the IRQ Stack}
         if IRQ_STACK_BASE[Count] <> 0 then IRQ_STACK_BASE[Count]:=IRQ_STACK_BASE[Count] + (IRQ_STACK_SIZE - STACK_MIN_ALIGNMENT);
        end;

       {Create FIQ Stack}
       if FIQ_ENABLED and FIQ_STACK_ENABLED then
        begin
         FIQ_STACK_BASE[Count]:=PtrUInt(AllocAlignedMem(FIQ_STACK_SIZE,STACK_MIN_ALIGNMENT));
         {Get the top of the FIQ Stack}
         if FIQ_STACK_BASE[Count] <> 0 then FIQ_STACK_BASE[Count]:=FIQ_STACK_BASE[Count] + (FIQ_STACK_SIZE - STACK_MIN_ALIGNMENT);
        end;

       {Create SWI Stack}
       if SWI_ENABLED and SWI_STACK_ENABLED then
        begin
         SWI_STACK_BASE[Count]:=PtrUInt(AllocAlignedMem(SWI_STACK_SIZE,STACK_MIN_ALIGNMENT));
         {Get the top of the SWI Stack}
         if SWI_STACK_BASE[Count] <> 0 then SWI_STACK_BASE[Count]:=SWI_STACK_BASE[Count] + (SWI_STACK_SIZE - STACK_MIN_ALIGNMENT);
        end;

       {Create ABORT Stack}
       if ABORT_ENABLED and ABORT_STACK_ENABLED then
        begin
         ABORT_STACK_BASE[Count]:=PtrUInt(AllocAlignedMem(ABORT_STACK_SIZE,STACK_MIN_ALIGNMENT));
         {Get the top of the ABORT Stack}
         if ABORT_STACK_BASE[Count] <> 0 then ABORT_STACK_BASE[Count]:=ABORT_STACK_BASE[Count] + (ABORT_STACK_SIZE - STACK_MIN_ALIGNMENT);
        end;

       {Create UNDEFINED Stack}
       if UNDEFINED_ENABLED and UNDEFINED_STACK_ENABLED then
        begin
         UNDEFINED_STACK_BASE[Count]:=PtrUInt(AllocAlignedMem(UNDEFINED_STACK_SIZE,STACK_MIN_ALIGNMENT));
         {Get the top of the UNDEFINED Stack}
         if UNDEFINED_STACK_BASE[Count] <> 0 then UNDEFINED_STACK_BASE[Count]:=UNDEFINED_STACK_BASE[Count] + (UNDEFINED_STACK_SIZE - STACK_MIN_ALIGNMENT);
        end;

       {Create Boot Thread}
       if THREAD_SHARED_MEMORY then
        begin
         ThreadEntry:=AllocSharedMem(SizeOf(TThreadEntry));
        end
       else
        begin
         ThreadEntry:=AllocMem(SizeOf(TThreadEntry));
        end;
       if ThreadEntry <> nil then
        begin
         if SCHEDULER_FIQ_ENABLED then
          begin
           {Setup FIQ Thread}
           {Thread Properties}
           ThreadEntry.Signature:=THREAD_SIGNATURE;
           ThreadEntry.State:=THREAD_STATE_RUNNING;
           ThreadEntry.Flags:=THREAD_FLAG_NONE;
           ThreadEntry.Priority:=THREAD_PRIORITY_NORMAL;           {Thread switches itself to THREAD_PRIORITY_NONE after initialization}
           ThreadEntry.Affinity:=(1 shl Count);                    {Must always run on the same Secondary CPU}
           ThreadEntry.StackBase:=Pointer(BOOT_STACK_BASE[Count]); {Assign the Boot Stack to the Boot Thread}
           ThreadEntry.StackSize:=BOOT_STACK_SIZE;                 {Use the Boot Stack size not the FIQ Stack size}
           StrLCopy(ThreadEntry.Name,PChar(FIQ_THREAD_NAME + IntToStr(Count)),THREAD_NAME_LENGTH - 1);
           ThreadEntry.Lock:=SpinCreate;
           ThreadEntry.Parent:=INVALID_HANDLE_VALUE;
           ThreadEntry.Messages.Maximum:=THREAD_MESSAGES_MAXIMUM;
           ThreadEntry.Messages.List:=AllocMem(SizeOf(TMessage) * ThreadEntry.Messages.Maximum);
           ThreadEntry.TlsPointer:=AllocMem(ThreadVarBlockSize);
           ThreadEntry.ExitCode:=ERROR_SUCCESS;
           ThreadEntry.LastError:=ERROR_SUCCESS;
           ThreadEntry.Locale:=LOCALE_DEFAULT;
           {Internal Properties}
           ThreadEntry.CurrentCPU:=Count;                          {Must always run on the same Secondary CPU}
           ThreadEntry.StackPointer:=nil;                          {Set on first context switch}
           ThreadEntry.TargetCPU:=Count;                           {Must always run on the same Secondary CPU}
           ThreadEntry.TargetPriority:=THREAD_PRIORITY_NORMAL;
           ThreadEntry.List:=ListCreateEx(LIST_TYPE_WAIT_THREAD,SchedulerGetListFlags(LIST_TYPE_WAIT_THREAD));  {INVALID_HANDLE_VALUE;} {Preallocated on threads to prevent IRQ/FIQ deadlocks in ListCreate}
           ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
           ThreadEntry.WaitLists:=nil;
           ThreadEntry.WaitResult:=ERROR_SUCCESS;
           ThreadEntry.ReceiveResult:=ERROR_SUCCESS;
           ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
           ThreadEntry.ListElement.Thread:=TThreadHandle(ThreadEntry);
           ThreadEntry.QueueElement.Thread:=TThreadHandle(ThreadEntry);
           {Statistics Properties}
           ThreadEntry.CreateTime:=ClockGetTime;
           ThreadEntry.ExitTime:=TIME_TICKS_TO_1899;
           ThreadEntry.KernelTime:=TIME_TICKS_TO_1899;
           ThreadEntry.SwitchCount:=0;

           {Insert Thread entry}
           if SpinLock(ThreadTableLock) = ERROR_SUCCESS then
            begin
             {Link Thread entry}
             if ThreadTable = nil then
              begin
               ThreadTable:=ThreadEntry;
              end
             else
              begin
               ThreadEntry.Next:=ThreadTable;
               ThreadTable.Prev:=ThreadEntry;
               ThreadTable:=ThreadEntry;
              end;

             {Increment Thread Count}
             Inc(ThreadTableCount);

             {Set Boot Thread}
             BOOT_THREAD_HANDLE[Count]:=TThreadHandle(ThreadEntry);

             {Set Thread Handle}
             FIQ_THREAD_HANDLE[Count]:=TThreadHandle(ThreadEntry);

             SpinUnlock(ThreadTableLock);
            end
           else
            begin
             {Invalidate Boot Thread}
             BOOT_THREAD_HANDLE[Count]:=INVALID_HANDLE_VALUE;

             {Invalidate Thread Handle}
             FIQ_THREAD_HANDLE[Count]:=INVALID_HANDLE_VALUE;
            end;
          end
         else
          begin
           {Setup IRQ Thread}
           {Thread Properties}
           ThreadEntry.Signature:=THREAD_SIGNATURE;
           ThreadEntry.State:=THREAD_STATE_RUNNING;
           ThreadEntry.Flags:=THREAD_FLAG_NONE;
           ThreadEntry.Priority:=THREAD_PRIORITY_NORMAL;           {Thread switches itself to THREAD_PRIORITY_NONE after initialization}
           ThreadEntry.Affinity:=(1 shl Count);                    {Must always run on the same Secondary CPU}
           ThreadEntry.StackBase:=Pointer(BOOT_STACK_BASE[Count]); {Assign the Boot Stack to the Boot Thread}
           ThreadEntry.StackSize:=BOOT_STACK_SIZE;              {Use the Boot Stack size not the IRQ Stack size}
           StrLCopy(ThreadEntry.Name,PChar(IRQ_THREAD_NAME + IntToStr(Count)),THREAD_NAME_LENGTH - 1);
           ThreadEntry.Lock:=SpinCreate;
           ThreadEntry.Parent:=INVALID_HANDLE_VALUE;
           ThreadEntry.Messages.Maximum:=THREAD_MESSAGES_MAXIMUM;
           ThreadEntry.Messages.List:=AllocMem(SizeOf(TMessage) * ThreadEntry.Messages.Maximum);
           ThreadEntry.TlsPointer:=AllocMem(ThreadVarBlockSize);
           ThreadEntry.ExitCode:=ERROR_SUCCESS;
           ThreadEntry.LastError:=ERROR_SUCCESS;
           ThreadEntry.Locale:=LOCALE_DEFAULT;
           {Internal Properties}
           ThreadEntry.CurrentCPU:=Count;                          {Must always run on the same Secondary CPU}
           ThreadEntry.StackPointer:=nil;                          {Set on first context switch}
           ThreadEntry.TargetCPU:=Count;                           {Must always run on the same Secondary CPU}
           ThreadEntry.TargetPriority:=THREAD_PRIORITY_NORMAL;
           ThreadEntry.List:=ListCreateEx(LIST_TYPE_WAIT_THREAD,SchedulerGetListFlags(LIST_TYPE_WAIT_THREAD));  {INVALID_HANDLE_VALUE;} {Preallocated on threads to prevent IRQ/FIQ deadlocks in ListCreate}
           ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
           ThreadEntry.WaitLists:=nil;
           ThreadEntry.WaitResult:=ERROR_SUCCESS;
           ThreadEntry.ReceiveResult:=ERROR_SUCCESS;
           ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
           ThreadEntry.ListElement.Thread:=TThreadHandle(ThreadEntry);
           ThreadEntry.QueueElement.Thread:=TThreadHandle(ThreadEntry);
           {Statistics Properties}
           ThreadEntry.CreateTime:=ClockGetTime;
           ThreadEntry.ExitTime:=TIME_TICKS_TO_1899;
           ThreadEntry.KernelTime:=TIME_TICKS_TO_1899;
           ThreadEntry.SwitchCount:=0;

           {Insert Thread entry}
           if SpinLock(ThreadTableLock) = ERROR_SUCCESS then
            begin
             {Link Thread entry}
             if ThreadTable = nil then
              begin
               ThreadTable:=ThreadEntry;
              end
             else
              begin
               ThreadEntry.Next:=ThreadTable;
               ThreadTable.Prev:=ThreadEntry;
               ThreadTable:=ThreadEntry;
              end;

             {Increment Thread Count}
             Inc(ThreadTableCount);

             {Set Boot Thread}
             BOOT_THREAD_HANDLE[Count]:=TThreadHandle(ThreadEntry);

             {Set Thread Handle}
             IRQ_THREAD_HANDLE[Count]:=TThreadHandle(ThreadEntry);

             SpinUnlock(ThreadTableLock);
            end
           else
            begin
             {Invalidate Boot Thread}
             BOOT_THREAD_HANDLE[Count]:=INVALID_HANDLE_VALUE;

             {Invalidate Thread Handle}
             IRQ_THREAD_HANDLE[Count]:=INVALID_HANDLE_VALUE;
            end;
          end;

         {Check Boot Thread}
         if BOOT_THREAD_HANDLE[Count] <> INVALID_HANDLE_VALUE then
          begin

           {Boot Secondary CPU}
           SecondaryBoot(Count);
          end
         else
          begin
           {$IFDEF THREAD_DEBUG}
           if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to initialize boot thread, Secondary CPU cannot start');
           {$ENDIF}
          end;
        end
       else
        begin
         {$IFDEF THREAD_DEBUG}
         if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create boot thread, Secondary CPU cannot start');
         {$ENDIF}
        end;
      end;
    end;
  end;

 SecondaryInitialized:=True;
end;

{==============================================================================}

procedure SecondaryBoot(CPUID:LongWord);
{Boot the specified secondary CPU (Where Applicable)}

{Note: Called only during system startup}
begin
 {}
 {Clean Cache for Secondary CPU Boot}
 CleanDataCache;

 {Check the Handler}
 if Assigned(SecondaryBootHandler) then
  begin
   {Call the Handler}
   SecondaryBootHandler(CPUID);
  end;
end;

{==============================================================================}

procedure SecondaryStart(CPUID:LongWord);
{Startup procedure for secondary CPUs (Where Applicable)}
{Note: The Secondary Boot procedure should have already cleared L1 cache, enabled FPU, MMU, Vectors and PageTables before
       calling this function. The thread id of the IRQ or FIQ should also have been loaded into the appropriate registers}

{Note: Called only during system startup}
var
 Thread:TThreadHandle;
begin
 {}
 {Check CPU}
 if CPUID > (CPUGetCount - 1) then Exit;

 {Wait for Initialized}
 while not(SecondaryInitialized) do
  begin
   {Nothing}
  end;

 {Check for Disable}
 if SCHEDULER_SECONDARY_DISABLED then
  begin
   {Halt CPU (Forever)}
   Halt;
  end;

 {Start Scheduler for CPU}
 SchedulerStart(CPUID);

 if SCHEDULER_FIQ_ENABLED then
  begin
   {Create IRQ Thread (FIQ Thread is the Boot Thread)}
   IRQ_THREAD_HANDLE[CPUID]:=SysBeginThreadEx(nil,IRQ_STACK_SIZE,IRQExecute,nil,0,THREAD_PRIORITY_NORMAL,(1 shl CPUID),CPUID,PChar(IRQ_THREAD_NAME + IntToStr(CPUID)),Thread);
   if IRQ_THREAD_HANDLE[CPUID] = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create IRQ thread, Secondary Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup IRQ Thread}
   {Name}
   ThreadSetName(IRQ_THREAD_HANDLE[CPUID],IRQ_THREAD_NAME + IntToStr(CPUID));
   {Priority (Thread switches itself to THREAD_PRIORITY_NONE after initialization)}
   ThreadSetPriority(IRQ_THREAD_HANDLE[CPUID],THREAD_PRIORITY_NORMAL);
   {Affinity (Must always run on the same Secondary CPU)}
   ThreadSetAffinity(IRQ_THREAD_HANDLE[CPUID],(1 shl CPUID));
  end
 else
  begin
   {Create FIQ Thread (IRQ Thread is the Boot Thread)}
   if FIQ_ENABLED then
    begin
     FIQ_THREAD_HANDLE[CPUID]:=SysBeginThreadEx(nil,FIQ_STACK_SIZE,FIQExecute,nil,0,THREAD_PRIORITY_NORMAL,(1 shl CPUID),CPUID,PChar(FIQ_THREAD_NAME + IntToStr(CPUID)),Thread);
     if FIQ_THREAD_HANDLE[CPUID] = INVALID_HANDLE_VALUE then
      begin
       {$IFDEF THREAD_DEBUG}
       if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create FIQ thread, Secondary Halted');
       {$ENDIF}

       Halt;
      end;
     {Setup FIQ Thread}
     {Name}
     ThreadSetName(FIQ_THREAD_HANDLE[CPUID],FIQ_THREAD_NAME + IntToStr(CPUID));
     {Priority (Thread switches itself to THREAD_PRIORITY_NONE after initialization)}
     ThreadSetPriority(FIQ_THREAD_HANDLE[CPUID],THREAD_PRIORITY_NORMAL);
     {Affinity (Must always run on the same Secondary CPU)}
     ThreadSetAffinity(FIQ_THREAD_HANDLE[CPUID],(1 shl CPUID));
    end;
  end;

 {Create SWI Thread}
 if SWI_ENABLED then
  begin
   SWI_THREAD_HANDLE[CPUID]:=SysBeginThreadEx(nil,SWI_STACK_SIZE,SWIExecute,nil,0,THREAD_PRIORITY_NORMAL,(1 shl CPUID),CPUID,PChar(SWI_THREAD_NAME + IntToStr(CPUID)),Thread);
   if SWI_THREAD_HANDLE[CPUID] = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF THREAD_DEBUG}
     if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create SWI thread, Secondary Halted');
     {$ENDIF}

     Halt;
    end;
   {Setup SWI Thread}
   {Name}
   ThreadSetName(SWI_THREAD_HANDLE[CPUID],SWI_THREAD_NAME + IntToStr(CPUID));
   {Priority (Thread switches itself to THREAD_PRIORITY_NONE after initialization)}
   ThreadSetPriority(SWI_THREAD_HANDLE[CPUID],THREAD_PRIORITY_NORMAL);
   {Affinity (Must always run on the same Secondary CPU)}
   ThreadSetAffinity(SWI_THREAD_HANDLE[CPUID],(1 shl CPUID));
  end;

 {Create Idle Thread}
 IDLE_THREAD_HANDLE[CPUID]:=SysBeginThreadEx(nil,IDLE_STACK_SIZE,IdleExecute,nil,0,THREAD_PRIORITY_IDLE,(1 shl CPUID),CPUID,PChar(IDLE_THREAD_NAME + IntToStr(CPUID)),Thread);
 if IDLE_THREAD_HANDLE[CPUID] = INVALID_HANDLE_VALUE then
  begin
   {$IFDEF THREAD_DEBUG}
   if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to create idle thread, Secondary Halted');
   {$ENDIF}

   Halt;
  end;
 {Setup Idle Thread}
 {Name}
 ThreadSetName(IDLE_THREAD_HANDLE[CPUID],IDLE_THREAD_NAME + IntToStr(CPUID));
 {Priority}
 ThreadSetPriority(IDLE_THREAD_HANDLE[CPUID],THREAD_PRIORITY_IDLE);
 {Affinity (Must always run on the same Secondary CPU)}
 ThreadSetAffinity(IDLE_THREAD_HANDLE[CPUID],(1 shl CPUID));

 {Wait for Ready}
 while SCHEDULER_SECONDARY_WAIT and not(SysInitializationCompleted) do
  begin
   {$IFDEF SCHEDULER_DEBUG}
   Inc(SchedulerSecondaryWaitCounter[CPUID]);
   {$ENDIF}

   {Nothing}
  end;

 {Synchronization Barrier}
 DataSynchronizationBarrier;

 if SCHEDULER_FIQ_ENABLED then
  begin
   {Call the FIQ Thread Handler}
   FIQExecute(nil);
  end
 else
  begin
   {Call the IRQ Thread Handler}
   IRQExecute(nil);
  end;
end;

{==============================================================================}

function IRQExecute(Parameter:Pointer):PtrInt;
{IRQ thread function}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=0;

 {When IRQ threads are created their priority is set to normal so that they will
  be scheduled initially in order to complete their initialization

  The thread needs to run to here in order to initialize ThreadVars, Exceptions etc
  but will never run again once they reach this routine

  Once here the threads will perform the secondary CPU init (where applicable),
  enable IRQ and then change their priority to none so that they will never be
  rescheduled again

  The IRQ handler will switch to the appropriate IRQ thread and stack for the CPU
  in order to execute the interrupt handlers}

 {Note: IRQExecute does not have a default exception handler, this is intentional
  as the IRQ threads as never scheduled and only execute on an IRQ which then runs
  on a different stack to the actual thread}

 {Inititalize Secondary CPUs}
 if not(SCHEDULER_FIQ_ENABLED) and (CPUGetCurrent = SCHEDULER_CPU_BOOT) then SecondaryInit;

 {Mark Initialization Completed}
 if not(SCHEDULER_FIQ_ENABLED) then InitializationCompleted[CPUGetCurrent]:=True;

 {Enable Abort}
 if not(SCHEDULER_FIQ_ENABLED) then EnableAbort;

 {Enable IRQ}
 EnableIRQ;

 {Set Priority}
 ThreadSetPriority(ThreadGetCurrent,THREAD_PRIORITY_NONE);

 {Reschedule immediately}
 SchedulerReschedule(True);

 {Never Executed}
 while True do
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function FIQExecute(Parameter:Pointer):PtrInt;
{FIQ thread function}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=0;

 {When FIQ threads are created their priority is set to normal so that they will
  be scheduled initially in order to complete their initialization

  The thread needs to run to here in order to initialize ThreadVars, Exceptions etc
  but will never run again once they reach this routine

  Once here the threads will perform the secondary CPU init (where applicable),
  enable FIQ and then change their priority to none so that they will never be
  rescheduled again

  The FIQ handler will switch to the appropriate FIQ thread and stack for the CPU
  in order to execute the interrupt handlers}

 {Note: FIQExecute does not have a default exception handler, this is intentional
  as the FIQ threads as never scheduled and only execute on an FIQ which then runs
  on a different stack to the actual thread}

 {Inititalize Secondary CPUs}
 if SCHEDULER_FIQ_ENABLED and (CPUGetCurrent = SCHEDULER_CPU_BOOT) then SecondaryInit;

 {Mark Initialization Completed}
 if SCHEDULER_FIQ_ENABLED then InitializationCompleted[CPUGetCurrent]:=True;

 {Enable Abort}
 if SCHEDULER_FIQ_ENABLED then EnableAbort;

 {Enable FIQ}
 EnableFIQ;

 {Set Priority}
 ThreadSetPriority(ThreadGetCurrent,THREAD_PRIORITY_NONE);

 {Reschedule immediately}
 SchedulerReschedule(True);

 {Never Executed}
 while True do
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function SWIExecute(Parameter:Pointer):PtrInt;
{SWI thread function}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=0;

 {When SWI threads are created their priority is set to normal so that they will
  be scheduled initially in order to complete their initialization

  The thread needs to run to here in order to initialize ThreadVars, Exceptions etc
  but will never run again once they reach this routine

  Once here the threads will change their priority to none so that they will never be
  rescheduled again

  The SWI handler will switch to the appropriate SWI thread and stack for the CPU
  in order to execute the software interrupt handlers}

 {Note: SWIExecute does not have a default exception handler, this is intentional
  as the SWI threads as never scheduled and only execute on an SWI call which then
  runs on a different stack to the actual thread}

 {Set Priority}
 ThreadSetPriority(ThreadGetCurrent,THREAD_PRIORITY_NONE);

 {Reschedule immediately}
 SchedulerReschedule(True);

 {Never Executed}
 while True do
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function IdleExecute(Parameter:Pointer):PtrInt;
{Idle thread function}

{Note: Not intended to be called directly by applications}
var
 CurrentCPU:LongWord;
begin
 {}
 Result:=0;

 {Idle threads are responsible for calculating CPU utilization and also serve
  as an always ready thread to be run when no other threads are ready

  For this reason Idle threads must never Sleep, Yield or Wait and must never
  acquire any Locks or other resources

  CPU utilization is based on the number of loops this routine can complete in
  a second and is independent for each CPU in the system

  The value UtilizationCurrent is set to the value of SCHEDULER_IDLE_PER_SECOND
  by the scheduler interrupt and then decremented on each loop of this routine

  At the end of each second the scheduler interrupt saves the value for the last
  second to UtilizationLast and resets the value of UtilizationCurrent again}

 {Note: IdleExecute does not have a default exception handler, this is intentional
  as the idle threads must never sleep, yield or wait}

 {Get Current CPU}
 CurrentCPU:=CPUGetCurrent;

 while True do
  begin
   {Delay}
   MicrosecondDelayEx(SCHEDULER_IDLE_OFFSET * 1000,SCHEDULER_IDLE_WAIT); {Offset * 1ms}

   {Decrement Utilization}
   Dec(UtilizationCurrent[CurrentCPU],SCHEDULER_IDLE_OFFSET);
  end;
end;

{==============================================================================}

function MainExecute(Parameter:Pointer):PtrInt;
{Main thread function}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=0;
 try
  {Note: The RTL will call the Initialization section of each unit at the start of
         PASCALMAIN. Some units (such as Platform, Threads, Console etc) have already
         been initialized to get to this point. The Initialized flag will catch that
         and prevent them being called twice}

  {Call Pascal Main which will call initialization of all units}
  PASCALMAIN;

  {Note: The RTL will call the Finalization section of each unit at the end of
         PASCALMAIN. It is not expected that this will ever happen but it may
         be part of a specific design to shutdown the system (leaving only the
         Idle, IRQ and FIQ threads) before powering off}
 except
  on E: Exception do
   begin
    if THREAD_LOG_ENABLED then ThreadLogError('MainThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end;
end;

{==============================================================================}

function TimerExecute(Parameter:Pointer):PtrInt;
{Timer thread function}

{Note: Not intended to be called directly by applications}
var
 Ticks:Integer;
 Flags:LongWord;
 Data:Pointer;
 Event:TTimerEvent;

 Message:TMessage;
 Timer:TTimerHandle;
 TimerEntry:PTimerEntry;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=0;
 try
  {The timer threads are responsible for calling timer callbacks on each timer
   trigger

   Enabled timers are held on a delta ascending ordered list which is checked
   and decremented on each clock tick by the clock interrupt handler

   When the first key in the list reaches zero the timer is dequeued from the
   list and a message sent to the next available timer thread to call the event
   registered for that timer

   The timer thread first checks the event and data and then if the timer is
   enabled and marked as reschedule it is inserted back into the list in order
   based on the timer interval

   If the timer is not marked as rescheduled it is set to disabled and the owner
   will need to call timer enable in order to trigger the event again

   The timer thread then calls the registered event, passing the data parameter,
   and returns to waiting for messages}

  while True do
   begin
    {Wait to Receive Message}
    FillChar(Message,SizeOf(TMessage),0);
    if MessageslotReceive(TimerMessageslot,Message) = ERROR_SUCCESS then
     begin
      {Process Message}
      Timer:=TTimerHandle(Message.Msg);

      {Check Timer}
      if Timer <> INVALID_HANDLE_VALUE then
       begin
        {Check the Handle}
        TimerEntry:=PTimerEntry(Timer);
        if (TimerEntry <> nil) and (TimerEntry.Signature = TIMER_SIGNATURE) then
         begin
          {Acquire the Lock}
          if SpinLock(TimerEntry.Lock) = ERROR_SUCCESS then
           begin
            try
             {Get the Event and Data}
             Event:=TimerEntry.Event;
             Data:=TimerEntry.Data;

             {Get the Flags}
             Flags:=TimerEntry.Flags;

             {Check the State}
             if TimerEntry.State = TIMER_STATE_ENABLED then
              begin
               {Check the Flags}
               if (TimerEntry.Flags and TIMER_FLAG_RESCHEDULE) <> 0 then
                begin
                 {Calculate Ticks}
                 Ticks:=CLOCK_TICKS_PER_MILLISECOND * TimerEntry.Interval;

                 {Insert in List}
                 TimerInsertKey(Timer,Ticks);
                end
               else
                begin
                 {Set State}
                 TimerEntry.State:=TIMER_STATE_DISABLED;
                end;
              end;
            finally
             {Release the Lock}
             SpinUnlock(TimerEntry.Lock);
            end;

            {Check the Flags}
            if (Flags and TIMER_FLAG_WORKER) = 0 then
             begin
              {Check the Event}
              if Assigned(Event) then
               begin
                {Call the Event}
                Event(Data);
               end;
             end
            else
             begin
              {Get Worker Request}
              WorkerRequest:=PWorkerRequest(PtrUInt(TimerEntry) + PtrUInt(SizeOf(TTimerEntry)));
              if (WorkerRequest <> nil) and (WorkerRequest.Signature = WORKER_SIGNATURE) then
               begin
                {Submit Worker Request}
                FillChar(Message,SizeOf(TMessage),0);
                Message.Msg:=PtrUInt(WorkerRequest);

                {Check the Flags}
                if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
                 begin
                  MessageslotSend(WorkerMessageslot,Message);
                 end
                else
                 begin
                  MessageslotSend(WorkerPriorityMessageslot,Message);
                 end;

                {Worker Request will NOT be freed by WorkerExecute/WorkerPriorityExecute}
               end;
             end;
           end;
         end;
       end;
     end;
   end;
 except
  on E: Exception do
   begin
    if THREAD_LOG_ENABLED then ThreadLogError('TimerThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end;
end;

{==============================================================================}

function WorkerExecute(Parameter:Pointer):PtrInt;
{Worker thread function}

{Note: Not intended to be called directly by applications}
var
 Data:Pointer;
 Task:TWorkerTask;
 Callback:TWorkerCallback;

 Flags:LongWord;
 Message:TMessage;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=0;
 try
  {The worker threads are responsible for calling worker tasks and callbacks

   Worker requests are created by WorkerSchedule and either submitted immediately
   to the worker threads for processing or scheduled on a timer for processing at
   a later time.

   For worker requests that are scheduled for later, the timer event calls WorkerTimer
   which then submits the worker request to the worker threads and either resubmits
   the timer or cleans up depending on the flags passed to WorkerSchedule.

   Once a request is received the worker thread then calls the registered task, passing
   the data parameter, and when complete calls the callback and returns to waiting for
   messages}

  {Update Worker Count}
  if SpinLock(WorkerThreadLock) = ERROR_SUCCESS then
   begin
    Inc(WorkerThreadCount);

    SpinUnlock(WorkerThreadLock);
   end;

  while True do
   begin
    {Wait to Receive Message}
    FillChar(Message,SizeOf(TMessage),0);
    if MessageslotReceive(WorkerMessageslot,Message) = ERROR_SUCCESS then
     begin
      {Process Message}
      WorkerRequest:=PWorkerRequest(Message.Msg);

      {Check Worker Request}
      if (WorkerRequest <> nil) and (WorkerRequest.Signature = WORKER_SIGNATURE)  then
       begin
        {Get the Task, Data and Callback}
        Task:=WorkerRequest.Task;
        Data:=WorkerRequest.Data;
        Callback:=WorkerRequest.Callback;

        {Get the Flags}
        Flags:=WorkerRequest.Flags;

        {Check Flags}
        if (Flags and WORKER_FLAG_NOFREE) = 0 then
         begin
          if (Flags and WORKER_FLAG_IRQ) <> 0 then
           begin
            {Free Worker Request}
            FreeIRQMem(WorkerRequest);
           end
          else if (Flags and WORKER_FLAG_FIQ) <> 0 then
           begin
            {Free Worker Request}
            FreeFIQMem(WorkerRequest);
           end
          else
           begin
            {Free Worker Request}
            FreeMem(WorkerRequest);
           end;
         end;

        {Check the Task}
        if Assigned(Task) then
         begin
          {Call the Task}
          Task(Data);

          {Check the Callback}
          if Assigned(Callback) then
           begin
            {Call the Callback}
            Callback(Data);
           end;
         end;

        {Check Flags}
        if (Flags and WORKER_FLAG_TERMINATE) <> 0 then
         begin
          {Terminate Thread}
          Break;
         end;
       end;
     end;
   end;

  {Update Worker Count}
  if SpinLock(WorkerThreadLock) = ERROR_SUCCESS then
   begin
    Dec(WorkerThreadCount);

    SpinUnlock(WorkerThreadLock);
   end;
 except
  on E: Exception do
   begin
    if THREAD_LOG_ENABLED then ThreadLogError('WorkerThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end;
end;

{==============================================================================}

function TimerPriorityExecute(Parameter:Pointer):PtrInt;
{Priority Timer thread function}

{Note: Not intended to be called directly by applications}
var
 Ticks:Integer;
 Flags:LongWord;
 Data:Pointer;
 Event:TTimerEvent;

 Message:TMessage;
 Timer:TTimerHandle;
 TimerEntry:PTimerEntry;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=0;
 try
  {High priority implementation of the timer threads, see TimerExecute() for more information}

  {Update Priority}
  ThreadSleep({$IFDEF SCHEDULER_YIELD_ALTERNATE}1{$ELSE}0{$ENDIF});

  while True do
   begin
    {Wait to Receive Message}
    FillChar(Message,SizeOf(TMessage),0);
    if MessageslotReceive(TimerPriorityMessageslot,Message) = ERROR_SUCCESS then
     begin
      {Process Message}
      Timer:=TTimerHandle(Message.Msg);

      {Check Timer}
      if Timer <> INVALID_HANDLE_VALUE then
       begin
        {Check the Handle}
        TimerEntry:=PTimerEntry(Timer);
        if (TimerEntry <> nil) and (TimerEntry.Signature = TIMER_SIGNATURE) then
         begin
          {Acquire the Lock}
          if SpinLock(TimerEntry.Lock) = ERROR_SUCCESS then
           begin
            try
             {Get the Event and Data}
             Event:=TimerEntry.Event;
             Data:=TimerEntry.Data;

             {Get the Flags}
             Flags:=TimerEntry.Flags;

             {Check the State}
             if TimerEntry.State = TIMER_STATE_ENABLED then
              begin
               {Check the Flags}
               if (TimerEntry.Flags and TIMER_FLAG_RESCHEDULE) <> 0 then
                begin
                 {Calculate Ticks}
                 Ticks:=CLOCK_TICKS_PER_MILLISECOND * TimerEntry.Interval;

                 {Insert in List}
                 TimerInsertKey(Timer,Ticks);
                end
               else
                begin
                 {Set State}
                 TimerEntry.State:=TIMER_STATE_DISABLED;
                end;
              end;
            finally
             {Release the Lock}
             SpinUnlock(TimerEntry.Lock);
            end;

            {Check the Flags}
            if (Flags and TIMER_FLAG_WORKER) = 0 then
             begin
              {Check the Event}
              if Assigned(Event) then
               begin
                {Call the Event}
                Event(Data);
               end;
             end
            else
             begin
              {Get Worker Request}
              WorkerRequest:=PWorkerRequest(PtrUInt(TimerEntry) + PtrUInt(SizeOf(TTimerEntry)));
              if (WorkerRequest <> nil) and (WorkerRequest.Signature = WORKER_SIGNATURE) then
               begin
                {Submit Worker Request}
                FillChar(Message,SizeOf(TMessage),0);
                Message.Msg:=PtrUInt(WorkerRequest);

                {Check the Flags}
                if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
                 begin
                  MessageslotSend(WorkerMessageslot,Message);
                 end
                else
                 begin
                  MessageslotSend(WorkerPriorityMessageslot,Message);
                 end;

                {Worker Request will NOT be freed by WorkerExecute/WorkerPriorityExecute}
               end;
             end;
           end;
         end;
       end;
     end;
   end;
 except
  on E: Exception do
   begin
    if THREAD_LOG_ENABLED then ThreadLogError('TimerPriorityThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end;
end;

{==============================================================================}

function WorkerPriorityExecute(Parameter:Pointer):PtrInt;
{Priority Worker thread function}

{Note: Not intended to be called directly by applications}
var
 Data:Pointer;
 Task:TWorkerTask;
 Callback:TWorkerCallback;

 Flags:LongWord;
 Message:TMessage;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=0;
 try
  {High priority implementation of the worker threads, see WorkerExecute() for more information}

  {Update Priority Worker Count}
  if SpinLock(WorkerPriorityThreadLock) = ERROR_SUCCESS then
   begin
    Inc(WorkerPriorityThreadCount);

    SpinUnlock(WorkerPriorityThreadLock);
   end;

  {Update Priority}
  ThreadSleep({$IFDEF SCHEDULER_YIELD_ALTERNATE}1{$ELSE}0{$ENDIF});

  while True do
   begin
    {Wait to Receive Message}
    FillChar(Message,SizeOf(TMessage),0);
    if MessageslotReceive(WorkerPriorityMessageslot,Message) = ERROR_SUCCESS then
     begin
      {Process Message}
      WorkerRequest:=PWorkerRequest(Message.Msg);

      {Check Worker Request}
      if (WorkerRequest <> nil) and (WorkerRequest.Signature = WORKER_SIGNATURE)  then
       begin
        {Get the Task, Data and Callback}
        Task:=WorkerRequest.Task;
        Data:=WorkerRequest.Data;
        Callback:=WorkerRequest.Callback;

        {Get the Flags}
        Flags:=WorkerRequest.Flags;

        {Check Flags}
        if (Flags and WORKER_FLAG_NOFREE) = 0 then
         begin
          if (Flags and WORKER_FLAG_IRQ) <> 0 then
           begin
            {Free Worker Request}
            FreeIRQMem(WorkerRequest);
           end
          else if (Flags and WORKER_FLAG_FIQ) <> 0 then
           begin
            {Free Worker Request}
            FreeFIQMem(WorkerRequest);
           end
          else
           begin
            {Free Worker Request}
            FreeMem(WorkerRequest);
           end;
         end;

        {Check the Task}
        if Assigned(Task) then
         begin
          {Call the Task}
          Task(Data);

          {Check the Callback}
          if Assigned(Callback) then
           begin
            {Call the Callback}
            Callback(Data);
           end;
         end;

        {Check Flags}
        if (Flags and WORKER_FLAG_TERMINATE) <> 0 then
         begin
          {Terminate Thread}
          Break;
         end;
       end;
     end;
   end;

  {Update Priority Worker Count}
  if SpinLock(WorkerPriorityThreadLock) = ERROR_SUCCESS then
   begin
    Dec(WorkerPriorityThreadCount);

    SpinUnlock(WorkerPriorityThreadLock);
   end;
 except
  on E: Exception do
   begin
    if THREAD_LOG_ENABLED then ThreadLogError('WorkerPriorityThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end;
end;

{==============================================================================}

function IdleCalibrate:LongWord;
{Calibrate the idle thread loop by counting the number of loops in 100ms}

{Note: Called only during system startup}
var
 Start:Int64;
 Target:Int64;
 Delay:LongWord;
 CurrentCPU:LongWord;
begin
 {}
 Result:=0;

 {Calculate Delay (Number of clock ticks in 100ms)}
 Delay:=CLOCK_CYCLES_PER_MILLISECOND * 100;

 {Get Starting Clock Count}
 Start:=ClockGetTotal;

 {Get Ending Clock Count}
 Target:=Start + Delay;

 {Check Count}
 if Target >= Start then
  begin
   {Get Current CPU}
   CurrentCPU:=CPUGetCurrent;

   {Set Counter}
   UtilizationCurrent[CurrentCPU]:=0;

   {Count Loops}
   while ClockGetTotal < Target do
    begin
     {Delay}
     MicrosecondDelayEx(SCHEDULER_IDLE_OFFSET * 1000,False); {Offset * 1ms} {Not SCHEDULER_IDLE_WAIT interrupts are not enabled}

     {Increment Count}
     Inc(UtilizationCurrent[CurrentCPU],SCHEDULER_IDLE_OFFSET);
    end;

   {Get 1 second result}
   Result:=UtilizationCurrent[CurrentCPU] * 10;

   {Reset Counter}
   UtilizationCurrent[CurrentCPU]:=SCHEDULER_IDLE_PER_SECOND;
  end
 else
  begin
   Result:=SCHEDULER_IDLE_PER_SECOND;
  end;
end;

{==============================================================================}
{==============================================================================}
{Spin Functions}
function SpinCreate:TSpinHandle; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Create and insert a new Spin entry}
{Return: Handle of new Spin entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=SpinCreateEx(False);
end;

{==============================================================================}

function SpinCreateEx(InitialOwner:Boolean):TSpinHandle;
{Create and insert a new Spin entry}
{InitialOwner: If true set the state of the spin to locked and the owner to the current thread}
{Return: Handle of new Spin entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create Spin entry}
 if SPIN_SHARED_MEMORY then
  begin
   SpinEntry:=AllocSharedMem(SizeOf(TSpinEntry));
  end
 else
  begin
   SpinEntry:=AllocMem(SizeOf(TSpinEntry));
  end;
 if SpinEntry = nil then Exit;

 {Setup Spin entry}
 SpinEntry.Signature:=SPIN_SIGNATURE;
 SpinEntry.State:=SPIN_STATE_UNLOCKED;
 SpinEntry.Mask:=0;
 SpinEntry.Owner:=INVALID_HANDLE_VALUE;

 {Setup Spin entry}
 if InitialOwner then
  begin
   SpinEntry.State:=SPIN_STATE_LOCKED;
   SpinEntry.Owner:=ThreadGetCurrent;
  end;

 {Insert Spin entry}
 if SpinLock(SpinTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Spin entry}
    if SpinTable = nil then
     begin
      SpinTable:=SpinEntry;
     end
    else
     begin
      SpinEntry.Next:=SpinTable;
      SpinTable.Prev:=SpinEntry;
      SpinTable:=SpinEntry;
     end;

    {Increment Spin Count}
    Inc(SpinTableCount);

    {Return Spin entry}
    Result:=TSpinHandle(SpinEntry);
   finally
    SpinUnlock(SpinTableLock);
   end;
  end
 else
  begin
   {Free Spin Entry}
   FreeMem(SpinEntry);
  end;
end;

{==============================================================================}

function SpinDestroy(Spin:TSpinHandle):LongWord;
{Destroy and remove an existing Spin entry}
{Spin: Handle of Spin entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
 PrevEntry:PSpinEntry;
 NextEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF SPIN_DEBUG}
 InterlockedIncrement(LongInt(SpinDestroyCounter));
 {$ENDIF}

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Acquire the Lock}
 Result:=SpinLock(Spin);
 if Result <> ERROR_SUCCESS then Exit;

 {Invalidate Spin entry}
 SpinEntry.Signature:=0;

 {Remove Spin entry}
 if SpinLock(SpinTableLock) = ERROR_SUCCESS then
  begin
   try
    {Unlink Spin entry}
    PrevEntry:=SpinEntry.Prev;
    NextEntry:=SpinEntry.Next;
    if PrevEntry = nil then
     begin
      SpinTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Spin Count}
    Dec(SpinTableCount);

    {Check Spin Count}
    if SpinTableCount < 1 then
     begin
      {$IFDEF THREAD_DEBUG}
      if THREAD_LOG_ENABLED then ThreadLogDebug('Final spin lock destroyed, System Halted');
      {$ENDIF}

      Halt;
     end;

    {Free Spin Entry}
    FreeMem(SpinEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(SpinTableLock);
   end;
  end
 else
  begin
   {Restore Spin entry}
   SpinEntry.Signature:=SPIN_SIGNATURE;

   {Release the Lock}
   Result:=SpinUnlock(Spin);
   if Result <> ERROR_SUCCESS then Exit;

   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SpinOwner(Spin:TSpinHandle):TThreadHandle;
{Get the current owner of an existing Spin entry}
{Spin: Handle of Spin entry to get owner for}
{Return: Handle of owning thread or INVALID_HANDLE_VALUE if not currently owned}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Get the Owner}
 Result:=SpinEntry.Owner;
end;

{==============================================================================}

function SpinLock(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Lock an existing Spin entry}
{Spin: Handle of Spin entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF SPIN_DEBUG}
 InterlockedIncrement(LongInt(SpinLockEntry));
 {$ENDIF}

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SpinDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SpinDeadlockCounter);
    end;
  end;
 if (SpinEntry.Owner = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SpinRecursionCounter);
   SpinRecursionThread:=SpinEntry.Owner;
  end;
 if (ThreadGetCurrent = IRQ_THREAD_HANDLE[CPUGetCurrent]) and (SCHEDULER_FIQ_ENABLED or InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SpinIRQThreadCounter);
  end;
 if (ThreadGetCurrent = FIQ_THREAD_HANDLE[CPUGetCurrent]) and (not(SCHEDULER_FIQ_ENABLED) or InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SpinFIQThreadCounter);
  end;
 if (ThreadGetCurrent = SWI_THREAD_HANDLE[CPUGetCurrent]) then
  begin
   Inc(SpinSWIThreadCounter);
  end;
 if (ThreadGetCurrent = IDLE_THREAD_HANDLE[CPUGetCurrent]) then
  begin
   Inc(SpinIdleThreadCounter);
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(SpinLockHandler) then
  begin
   {Use the Handler method}
   {Acquire the Lock}
   Result:=SpinLockHandler(SpinEntry);

   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinLockCounter));
   InterlockedIncrement(LongInt(SpinLockExit));
   {$ENDIF}
  end;
end;

{==============================================================================}

function SpinUnlock(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Unlock an existing Spin entry}
{Spin: Handle of Spin entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF SPIN_DEBUG}
 InterlockedIncrement(LongInt(SpinUnlockEntry));
 {$ENDIF}

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(SpinUnlockHandler) then
  begin
   {Use the Handler method}
   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinUnlockCounter));
   {$ENDIF}

   {Release the Lock}
   Result:=SpinUnlockHandler(SpinEntry);

   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinUnlockExit));
   {$ENDIF}
  end;
end;

{==============================================================================}

function SpinLockIRQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Lock an existing Spin entry, disable IRQ and save the previous IRQ state}
{Spin: Handle of Spin entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if (SpinEntry.Owner = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SpinRecursionCounter);
   SpinRecursionThread:=SpinEntry.Owner;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(SpinLockIRQHandler) then
  begin
   {Use the Handler method}
   {Acquire the Lock}
   Result:=SpinLockIRQHandler(SpinEntry);

   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinLockIRQCounter));
   {$ENDIF}
  end;
end;

{==============================================================================}

function SpinUnlockIRQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Unlock an existing Spin entry and restore the previous IRQ state}
{Spin: Handle of Spin entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(SpinUnlockIRQHandler) then
  begin
   {Use the Handler method}
   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinUnlockIRQCounter));
   {$ENDIF}

   {Release the Lock}
   Result:=SpinUnlockIRQHandler(SpinEntry);
  end;
end;

{==============================================================================}

function SpinLockFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Lock an existing Spin entry, disable FIQ and save the previous FIQ state}
{Spin: Handle of Spin entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if (SpinEntry.Owner = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SpinRecursionCounter);
   SpinRecursionThread:=SpinEntry.Owner;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(SpinLockFIQHandler) then
  begin
   {Use the Handler method}
   {Acquire the Lock}
   Result:=SpinLockFIQHandler(SpinEntry);

   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinLockFIQCounter));
   {$ENDIF}
  end;
end;

{==============================================================================}

function SpinUnlockFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Unlock an existing Spin entry and restore the previous FIQ state}
{Spin: Handle of Spin entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(SpinUnlockFIQHandler) then
  begin
   {Use the Handler method}
   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinUnlockFIQCounter));
   {$ENDIF}

   {Release the Lock}
   Result:=SpinUnlockFIQHandler(SpinEntry);
  end;
end;

{==============================================================================}

function SpinLockIRQFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Lock an existing Spin entry, disable IRQ and FIQ and save the previous IRQ and FIQ state}
{Spin: Handle of Spin entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if (SpinEntry.Owner = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SpinRecursionCounter);
   SpinRecursionThread:=SpinEntry.Owner;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(SpinLockIRQFIQHandler) then
  begin
   {Use the Handler method}
   {Acquire the Lock}
   Result:=SpinLockIRQFIQHandler(SpinEntry);

   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinLockIRQFIQCounter));
   {$ENDIF}
  end;
end;

{==============================================================================}

function SpinUnlockIRQFIQ(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Unlock an existing Spin entry and restore the previous IRQ and FIQ state}
{Spin: Handle of Spin entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(SpinUnlockIRQFIQHandler) then
  begin
   {Use the Handler method}
   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinUnlockIRQFIQCounter));
   {$ENDIF}

   {Release the Lock}
   Result:=SpinUnlockIRQFIQHandler(SpinEntry);
  end;
end;

{==============================================================================}

function SpinLockPreempt(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Lock an existing Spin entry, disable IRQ or IRQ/FIQ and save the previous IRQ or IRQ/FIQ state}
{Spin: Handle of Spin entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: This is a convenience wrapper which determines the appropriate SpinLock call to disable preemption}
begin
 {}
 {Check Scheduler FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   Result:=SpinLockIRQFIQ(Spin);
  end
 else
  begin
   Result:=SpinLockIRQ(Spin);
  end;
end;

{==============================================================================}

function SpinUnlockPreempt(Spin:TSpinHandle):LongWord; {$IFDEF SPIN_INLINE}inline;{$ENDIF}
{Unlock an existing Spin entry and restore the previous IRQ or IRQ/FIQ state}
{Spin: Handle of Spin entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: This is a convenience wrapper which determines the appropriate SpinUnlock call to enable preemption}
begin
 {}
 {Check Scheduler FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   Result:=SpinUnlockIRQFIQ(Spin);
  end
 else
  begin
   Result:=SpinUnlockIRQ(Spin);
  end;
end;

{==============================================================================}

function SpinCheckIRQ(Spin:TSpinHandle):Boolean;
{Check the mask that stores the previous IRQ state to determine if IRQ is enabled}
{Spin: Handle of Spin entry to check}
{Return: True if the mask would enable IRQ on restore, False if it would not}
{Note: The Spin entry must be locked by the current thread}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=False;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Lock}
 if SpinEntry.State <> SPIN_STATE_LOCKED then Exit;

 {Check the Owner}
 if SpinEntry.Owner <> ThreadGetCurrent then Exit;

 {Check the Handler}
 if Assigned(SpinCheckIRQHandler) then
  begin
   {Use the Handler method}
   Result:=SpinCheckIRQHandler(SpinEntry);
  end
 else
  begin
   {Use the Default method}
   {Nothing}
  end;
end;

{==============================================================================}

function SpinCheckFIQ(Spin:TSpinHandle):Boolean;
{Check the mask that stores the previous FIQ state to determine if FIQ is enabled}
{Spin: Handle of Spin entry to check}
{Return: True if the mask would enable FIQ on restore, False if it would not}
{Note: The Spin entry must be locked by the current thread}
var
 SpinEntry:PSpinEntry;
begin
 {}
 Result:=False;

 {Check Spin}
 if Spin = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SpinEntry:=PSpinEntry(Spin);
 if SpinEntry = nil then Exit;
 if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Lock}
 if SpinEntry.State <> SPIN_STATE_LOCKED then Exit;

 {Check the Owner}
 if SpinEntry.Owner <> ThreadGetCurrent then Exit;

 {Check the Handler}
 if Assigned(SpinCheckFIQHandler) then
  begin
   {Use the Handler method}
   Result:=SpinCheckFIQHandler(SpinEntry);
  end
 else
  begin
   {Use the Default method}
   {Nothing}
  end;
end;

{==============================================================================}

function SpinExchangeIRQ(Spin1,Spin2:TSpinHandle):LongWord;
{Exchange the previous IRQ state between two Spin entries}
{Spin1: Handle of first Spin entry}
{Spin2: Handle of second Spin entry}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Both Spin entries must be locked by the current thread}
var
 SpinEntry1:PSpinEntry;
 SpinEntry2:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin1}
 if Spin1 = INVALID_HANDLE_VALUE then Exit;

 {Check Spin2}
 if Spin2 = INVALID_HANDLE_VALUE then Exit;

 {Check first Handle}
 SpinEntry1:=PSpinEntry(Spin1);
 if SpinEntry1 = nil then Exit;
 if SpinEntry1.Signature <> SPIN_SIGNATURE then Exit;

 {Check second Handle}
 SpinEntry2:=PSpinEntry(Spin2);
 if SpinEntry2 = nil then Exit;
 if SpinEntry2.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Locks}
 Result:=ERROR_NOT_LOCKED;
 if SpinEntry1.State <> SPIN_STATE_LOCKED then Exit;
 if SpinEntry2.State <> SPIN_STATE_LOCKED then Exit;

 {Check the Owners}
 Result:=ERROR_NOT_OWNER;
 if SpinEntry1.Owner <> ThreadGetCurrent then Exit;
 if SpinEntry2.Owner <> ThreadGetCurrent then Exit;

 {Check the Handler}
 if Assigned(SpinExchangeIRQHandler) then
  begin
   {Use the Handler method}
   Result:=SpinExchangeIRQHandler(SpinEntry1,SpinEntry2);
  end
 else
  begin
   {Use the Default method}
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function SpinExchangeFIQ(Spin1,Spin2:TSpinHandle):LongWord;
{Exchange the previous FIQ state between two Spin entries}
{Spin1: Handle of first Spin entry}
{Spin2: Handle of second Spin entry}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Both Spin entries must be locked by the current thread}
var
 SpinEntry1:PSpinEntry;
 SpinEntry2:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin1}
 if Spin1 = INVALID_HANDLE_VALUE then Exit;

 {Check Spin2}
 if Spin2 = INVALID_HANDLE_VALUE then Exit;

 {Check first Handle}
 SpinEntry1:=PSpinEntry(Spin1);
 if SpinEntry1 = nil then Exit;
 if SpinEntry1.Signature <> SPIN_SIGNATURE then Exit;

 {Check second Handle}
 SpinEntry2:=PSpinEntry(Spin2);
 if SpinEntry2 = nil then Exit;
 if SpinEntry2.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Locks}
 Result:=ERROR_NOT_LOCKED;
 if SpinEntry1.State <> SPIN_STATE_LOCKED then Exit;
 if SpinEntry2.State <> SPIN_STATE_LOCKED then Exit;

 {Check the Owners}
 Result:=ERROR_NOT_OWNER;
 if SpinEntry1.Owner <> ThreadGetCurrent then Exit;
 if SpinEntry2.Owner <> ThreadGetCurrent then Exit;

 {Check the Handler}
 if Assigned(SpinExchangeFIQHandler) then
  begin
   {Use the Handler method}
   Result:=SpinExchangeFIQHandler(SpinEntry1,SpinEntry2);
  end
 else
  begin
   {Use the Default method}
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}
//Remove
function SpinMaskExchange(Spin1,Spin2:TSpinHandle):LongWord;
{Exchange the masks that store previous IRQ and FIQ state between two Spin entries}
{Spin1: Handle of first Spin entry}
{Spin2: Handle of second Spin entry}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Both Spin entries must be locked by the current thread}
var
 SpinMask:LongWord;
 SpinEntry1:PSpinEntry;
 SpinEntry2:PSpinEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Spin1}
 if Spin1 = INVALID_HANDLE_VALUE then Exit;

 {Check Spin2}
 if Spin2 = INVALID_HANDLE_VALUE then Exit;

 {Check first Handle}
 SpinEntry1:=PSpinEntry(Spin1);
 if SpinEntry1 = nil then Exit;
 if SpinEntry1.Signature <> SPIN_SIGNATURE then Exit;

 {Check second Handle}
 SpinEntry2:=PSpinEntry(Spin2);
 if SpinEntry2 = nil then Exit;
 if SpinEntry2.Signature <> SPIN_SIGNATURE then Exit;

 {Check the Locks}
 Result:=ERROR_NOT_LOCKED;
 if SpinEntry1.State <> SPIN_STATE_LOCKED then Exit;
 if SpinEntry2.State <> SPIN_STATE_LOCKED then Exit;

 {Check the Owners}
 Result:=ERROR_NOT_OWNER;
 if SpinEntry1.Owner <> ThreadGetCurrent then Exit;
 if SpinEntry2.Owner <> ThreadGetCurrent then Exit;

 {Exchange Masks}
 SpinMask:=SpinEntry1.Mask;
 SpinEntry1.Mask:=SpinEntry2.Mask;
 SpinEntry2.Mask:=SpinMask;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;
//Remove
{==============================================================================}

function SpinLockDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinLock function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinLock instead}
begin
 {}
 {Acquire the Lock}
 while InterlockedExchange(LongInt(SpinEntry.State),SPIN_STATE_LOCKED) <> SPIN_STATE_UNLOCKED do
  begin
   while SpinEntry.State <> SPIN_STATE_UNLOCKED do
    begin
     {Spin while waiting}

     {Check Signature}
     if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;
    end;
  end;

 {Memory Barrier}
 DataMemoryBarrier;

 {Set Owner}
 SpinEntry.Owner:=ThreadGetCurrent;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SpinUnlockDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinUnlock function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinUnlock instead}
begin
 {}
 {Check the Lock}
 if SpinEntry.State <> SPIN_STATE_LOCKED then
  begin
   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinUnlockNoLock));
   {$ENDIF}

   Result:=ERROR_NOT_LOCKED;
   Exit;
  end;

 {Check the Owner}
 if SpinEntry.Owner <> ThreadGetCurrent then
  begin
   {$IFDEF SPIN_DEBUG}
   InterlockedIncrement(LongInt(SpinUnlockNoOwner));
   {$ENDIF}

    Result:=ERROR_NOT_OWNER;
   Exit;
  end;

 {Release Owner}
 SpinEntry.Owner:=INVALID_HANDLE_VALUE;

 {Memory Barrier}
 DataMemoryBarrier;

 {Release the Lock}
 SpinEntry.State:=SPIN_STATE_UNLOCKED;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SpinLockIRQDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinLockIRQ function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinLockIRQ instead}
var
 Mask:TIRQMask;
begin
 {}
 {Save IRQ}
 Mask:=SaveIRQ;

 {Acquire the Lock}
 while InterlockedExchange(LongInt(SpinEntry.State),SPIN_STATE_LOCKED) <> SPIN_STATE_UNLOCKED do
  begin
   {Restore IRQ}
   RestoreIRQ(Mask);
   while SpinEntry.State <> SPIN_STATE_UNLOCKED do
    begin
     {Spin while waiting}

     {Check Signature}
     if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;
    end;

   {Save IRQ}
   Mask:=SaveIRQ;
  end;

 {Memory Barrier}
 DataMemoryBarrier;

 {Save the Mask}
 SpinEntry.Mask:=Mask;

 {Set Owner}
 SpinEntry.Owner:=ThreadGetCurrent;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SpinUnlockIRQDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinUnlockIRQ function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinUnlockIRQ instead}
var
 Mask:TIRQMask;
begin
 {}
 {Check the Lock}
 if SpinEntry.State <> SPIN_STATE_LOCKED then
  begin
   Result:=ERROR_NOT_LOCKED;
   Exit;
  end;

 {Check the Owner}
 if SpinEntry.Owner <> ThreadGetCurrent then
  begin
   Result:=ERROR_NOT_OWNER;
   Exit;
  end;

 {Release Owner}
 SpinEntry.Owner:=INVALID_HANDLE_VALUE;

 {Get the Mask}
 Mask:=SpinEntry.Mask;

 {Clear the Mask}
 SpinEntry.Mask:=0;

 {Memory Barrier}
 DataMemoryBarrier;

 {Release the Lock}
 SpinEntry.State:=SPIN_STATE_UNLOCKED;

 {Restore IRQ}
 RestoreIRQ(Mask);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SpinLockFIQDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinLockFIQ function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinLockFIQ instead}
var
 Mask:TFIQMask;
begin
 {}
 {Save FIQ}
 Mask:=SaveFIQ;

 {Acquire the Lock}
 while InterlockedExchange(LongInt(SpinEntry.State),SPIN_STATE_LOCKED) <> SPIN_STATE_UNLOCKED do
  begin
   {Restore FIQ}
   RestoreFIQ(Mask);
   while SpinEntry.State <> SPIN_STATE_UNLOCKED do
    begin
     {Spin while waiting}

     {Check Signature}
     if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;
    end;

   {Save FIQ}
   Mask:=SaveFIQ;
  end;

 {Memory Barrier}
 DataMemoryBarrier;

 {Save the Mask}
 SpinEntry.Mask:=Mask;

 {Set Owner}
 SpinEntry.Owner:=ThreadGetCurrent;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SpinUnlockFIQDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinUnlockFIQ function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinUnlockFIQ instead}
var
 Mask:TFIQMask;
begin
 {}
 {Check the Lock}
 if SpinEntry.State <> SPIN_STATE_LOCKED then
  begin
   Result:=ERROR_NOT_LOCKED;
   Exit;
  end;

 {Check the Owner}
 if SpinEntry.Owner <> ThreadGetCurrent then
  begin
   Result:=ERROR_NOT_OWNER;
   Exit;
  end;

 {Release Owner}
 SpinEntry.Owner:=INVALID_HANDLE_VALUE;

 {Get the Mask}
 Mask:=SpinEntry.Mask;

 {Clear the Mask}
 SpinEntry.Mask:=0;

 {Memory Barrier}
 DataMemoryBarrier;

 {Release the Lock}
 SpinEntry.State:=SPIN_STATE_UNLOCKED;

 {Restore FIQ}
 RestoreFIQ(Mask);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SpinLockIRQFIQDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinLockIRQFIQ function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinLockIRQFIQ instead}
var
 Mask:TIRQFIQMask;
begin
 {}
 {Save IRQFIQ}
 Mask:=SaveIRQFIQ;

 {Acquire the Lock}
 while InterlockedExchange(LongInt(SpinEntry.State),SPIN_STATE_LOCKED) <> SPIN_STATE_UNLOCKED do
  begin
   {Restore IRQFIQ}
   RestoreIRQFIQ(Mask);
   while SpinEntry.State <> SPIN_STATE_UNLOCKED do
    begin
     {Spin while waiting}

     {Check Signature}
     if SpinEntry.Signature <> SPIN_SIGNATURE then Exit;
    end;

   {Save IRQFIQ}
   Mask:=SaveIRQFIQ;
  end;

 {Memory Barrier}
 DataMemoryBarrier;

 {Save the Mask}
 SpinEntry.Mask:=Mask;

 {Set Owner}
 SpinEntry.Owner:=ThreadGetCurrent;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SpinUnlockIRQFIQDefault(SpinEntry:PSpinEntry):LongWord;
{Default version of SpinUnlockIRQFIQ function used if no handler is registered}
{Note: Not intended to be called directly by applications, use SpinUnlockIRQFIQ instead}
var
 Mask:TIRQFIQMask;
begin
 {}
 {Check the Lock}
 if SpinEntry.State <> SPIN_STATE_LOCKED then
  begin
   Result:=ERROR_NOT_LOCKED;
   Exit;
  end;

 {Check the Owner}
 if SpinEntry.Owner <> ThreadGetCurrent then
  begin
   Result:=ERROR_NOT_OWNER;
   Exit;
  end;

 {Release Owner}
 SpinEntry.Owner:=INVALID_HANDLE_VALUE;

 {Get the Mask}
 Mask:=SpinEntry.Mask;

 {Clear the Mask}
 SpinEntry.Mask:=0;

 {Memory Barrier}
 DataMemoryBarrier;

 {Release the Lock}
 SpinEntry.State:=SPIN_STATE_UNLOCKED;

 {Restore IRQFIQ}
 RestoreIRQFIQ(Mask);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{Mutex Functions}
function MutexCreate:TMutexHandle; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}
{Create and insert a new Mutex entry}
{Return: Handle of new Mutex entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_NONE);
end;

{==============================================================================}

function MutexCreateEx(InitialOwner:Boolean;SpinCount:LongWord;Flags:LongWord):TMutexHandle;
{Create and insert a new Mutex entry}
{InitialOwner: If true set the state of the mutex to locked and the owner to the current thread}
{SpinCount: The number of times the mutex will spin before yielding (Always 0 if CPU count equals 1)}
{Flags: The flags for the Mutex entry (eg MUTEX_FLAG_RECURSIVE)}
{Return: Handle of new Mutex entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 MutexEntry:PMutexEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create Mutex entry}
 if MUTEX_SHARED_MEMORY then
  begin
   MutexEntry:=AllocSharedMem(SizeOf(TMutexEntry));
  end
 else
  begin
   MutexEntry:=AllocMem(SizeOf(TMutexEntry));
  end;
 if MutexEntry = nil then Exit;

 {Setup Mutex entry}
 MutexEntry.Signature:=MUTEX_SIGNATURE;
 MutexEntry.State:=MUTEX_STATE_UNLOCKED;
 MutexEntry.Owner:=INVALID_HANDLE_VALUE;
 MutexEntry.Yield:=ThreadYield;
 MutexEntry.Count:=0;
 MutexEntry.Flags:=Flags;
 MutexEntry.SpinCount:=SpinCount;

 {Setup Mutex entry}
 if InitialOwner then
  begin
   MutexEntry.State:=MUTEX_STATE_LOCKED;
   MutexEntry.Owner:=ThreadGetCurrent;

   {Check Flags}
   if (MutexEntry.Flags and MUTEX_FLAG_RECURSIVE) <> 0 then
    begin
     MutexEntry.Count:=1;
    end;
  end;
 if SCHEDULER_CPU_COUNT = 1 then
  begin
   MutexEntry.SpinCount:=0;
  end;

 {Insert Mutex entry}
 if SpinLock(MutexTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Mutex entry}
    if MutexTable = nil then
     begin
      MutexTable:=MutexEntry;
     end
    else
     begin
      MutexEntry.Next:=MutexTable;
      MutexTable.Prev:=MutexEntry;
      MutexTable:=MutexEntry;
     end;

    {Increment Mutex Count}
    Inc(MutexTableCount);

    {Return Mutex entry}
    Result:=TMutexHandle(MutexEntry);
   finally
    SpinUnlock(MutexTableLock);
   end;
  end
 else
  begin
   {Free Mutex Entry}
   FreeMem(MutexEntry);
  end;
end;

{==============================================================================}

function MutexDestroy(Mutex:TMutexHandle):LongWord;
{Destroy and remove an existing Mutex entry}
{Mutex: Handle of Mutex entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MutexEntry:PMutexEntry;
 PrevEntry:PMutexEntry;
 NextEntry:PMutexEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF MUTEX_DEBUG}
 InterlockedIncrement(LongInt(MutexDestroyCounter));
 {$ENDIF}

 {Check Mutex}
 if Mutex = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MutexEntry:=PMutexEntry(Mutex);
 if MutexEntry = nil then Exit;
 if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;

 {Acquire the Lock}
 Result:=MutexLock(Mutex);
 if Result <> ERROR_SUCCESS then Exit;

 {Invalidate Mutex entry}
 MutexEntry.Signature:=0;

 {Remove Mutex entry}
 if SpinLock(MutexTableLock) = ERROR_SUCCESS then
  begin
   try
    {Unlink Mutex entry}
    PrevEntry:=MutexEntry.Prev;
    NextEntry:=MutexEntry.Next;
    if PrevEntry = nil then
     begin
      MutexTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Mutex Count}
    Dec(MutexTableCount);

    {Free Mutex Entry}
    FreeMem(MutexEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(MutexTableLock);
   end;
  end
 else
  begin
   {Restore Mutex entry}
   MutexEntry.Signature:=MUTEX_SIGNATURE;

   {Release the Lock}
   Result:=MutexUnlock(Mutex);
   if Result <> ERROR_SUCCESS then Exit;

   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MutexFlags(Mutex:TMutexHandle):LongWord;
{Get the current flags of an existing Mutex entry}
{Mutex: Handle of Mutex entry to get flags for}
{Return: Current flags or INVALID_HANDLE_VALUE on error}
var
 MutexEntry:PMutexEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Mutex}
 if Mutex = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MutexEntry:=PMutexEntry(Mutex);
 if MutexEntry = nil then Exit;
 if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;

 {Get the Flags}
 Result:=MutexEntry.Flags;
end;

{==============================================================================}

function MutexCount(Mutex:TMutexHandle):LongWord;
{Get the current lock count of an existing Mutex entry}
{Mutex: Mutex to get count for}
{Return: Current lock count or INVALID_HANDLE_VALUE on error}
{Note: Count is only valid if Flags includes MUTEX_FLAG_RECURSIVE}
var
 MutexEntry:PMutexEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Mutex}
 if Mutex = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MutexEntry:=PMutexEntry(Mutex);
 if MutexEntry = nil then Exit;
 if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;

 {Check the Flags}
 if (MutexEntry.Flags and MUTEX_FLAG_RECURSIVE) = 0 then
  begin
   {Return Default}
   Result:=0;
  end
 else
  begin
   {Get the Count}
   Result:=MutexEntry.Count;
  end;
end;

{==============================================================================}

function MutexOwner(Mutex:TMutexHandle):TThreadHandle;
{Get the current owner of an existing Mutex entry}
{Mutex: Handle of Mutex entry to get owner for}
{Return: Handle of owning thread or INVALID_HANDLE_VALUE if not currently owned}
var
 MutexEntry:PMutexEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Mutex}
 if Mutex = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MutexEntry:=PMutexEntry(Mutex);
 if MutexEntry = nil then Exit;
 if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;

 {Get the Owner}
 Result:=MutexEntry.Owner;
end;

{==============================================================================}

function MutexLock(Mutex:TMutexHandle):LongWord; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}
{Lock an existing Mutex entry}
{Mutex: Handle of Mutex entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MutexEntry:PMutexEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF MUTEX_DEBUG}
 InterlockedIncrement(LongInt(MutexLockEntry));
 {$ENDIF}

 {Check Mutex}
 if Mutex = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MutexEntry:=PMutexEntry(Mutex);
 if MutexEntry = nil then Exit;
 if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(MutexDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(MutexDeadlockCounter);
    end;
  end;
 if (MutexEntry.Owner = ThreadGetCurrent) and ((MutexEntry.Flags and MUTEX_FLAG_RECURSIVE) = 0) and (InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(MutexRecursionCounter);
   MutexRecursionThread:=MutexEntry.Owner;
  end;
 if (ThreadGetCurrent = IRQ_THREAD_HANDLE[CPUGetCurrent]) and (SCHEDULER_FIQ_ENABLED or InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(MutexIRQThreadCounter);
  end;
 if (ThreadGetCurrent = FIQ_THREAD_HANDLE[CPUGetCurrent]) and (not(SCHEDULER_FIQ_ENABLED) or InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(MutexFIQThreadCounter);
  end;
 if (ThreadGetCurrent = SWI_THREAD_HANDLE[CPUGetCurrent]) then
  begin
   Inc(MutexSWIThreadCounter);
  end;
 if (ThreadGetCurrent = IDLE_THREAD_HANDLE[CPUGetCurrent]) then
  begin
   Inc(MutexIdleThreadCounter);
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(MutexLockHandler) then
  begin
   {Call the Handler}
   Result:=MutexLockHandler(MutexEntry);

   {$IFDEF MUTEX_DEBUG}
   InterlockedIncrement(LongInt(MutexLockCounter));
   InterlockedIncrement(LongInt(MutexLockExit));
   {$ENDIF}
  end;
end;

{==============================================================================}

function MutexUnlock(Mutex:TMutexHandle):LongWord; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}
{Unlock an existing Mutex entry}
{Mutex: Handle of Mutex entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MutexEntry:PMutexEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF MUTEX_DEBUG}
 InterlockedIncrement(LongInt(MutexUnlockEntry));
 {$ENDIF}

 {Check Mutex}
 if Mutex = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MutexEntry:=PMutexEntry(Mutex);
 if MutexEntry = nil then Exit;
 if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(MutexUnlockHandler) then
  begin
   {$IFDEF MUTEX_DEBUG}
   InterlockedIncrement(LongInt(MutexUnlockCounter));
   {$ENDIF}

   {Call the Handler}
   Result:=MutexUnlockHandler(MutexEntry);

   {$IFDEF MUTEX_DEBUG}
   InterlockedIncrement(LongInt(MutexUnlockExit));
   {$ENDIF}
  end;
end;

{==============================================================================}

function MutexTryLock(Mutex:TMutexHandle):LongWord; {$IFDEF MUTEX_INLINE}inline;{$ENDIF}
{Try to lock an existing Mutex entry

 If the Mutex is not locked then lock it and mark the owner as the current thread

 If the Mutex is already locked then return immediately with an error and do not
 wait for it to be unlocked}
{Mutex: Mutex to try to lock}
{Return: ERROR_SUCCESS if completed, ERROR_LOCKED if already locked or another error code on failure}
var
 MutexEntry:PMutexEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Mutex}
 if Mutex = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MutexEntry:=PMutexEntry(Mutex);
 if MutexEntry = nil then Exit;
 if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(MutexTryLockHandler) then
  begin
   {Call the Handler}
   Result:=MutexTryLockHandler(MutexEntry);
  end;
end;

{==============================================================================}

function MutexLockDefault(MutexEntry:PMutexEntry):LongWord;
{Default version of MutexLock function used if no handler is registered}
{Note: Not intended to be called directly by applications, use MutexLock instead}
var
 Count:LongWord;
begin
 {}
 Count:=0;

 {Check the Flags}
 if (MutexEntry.Flags and MUTEX_FLAG_RECURSIVE) = 0 then
  begin
   {Acquire the Lock}
   while InterlockedExchange(LongInt(MutexEntry.State),MUTEX_STATE_LOCKED) <> MUTEX_STATE_UNLOCKED do
    begin
     while MutexEntry.State <> MUTEX_STATE_UNLOCKED do
      begin
       {Check Count}
       if Count < MutexEntry.SpinCount then
        begin
         {Increment Count}
         Inc(Count);
        end
       else
        begin
         {Yield while waiting}
         MutexEntry.Yield;
        end;

       {Check Signature}
       if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;
      end;
    end;

   {Memory Barrier}
   DataMemoryBarrier;

   {Set Owner}
   MutexEntry.Owner:=ThreadGetCurrent;

   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Check Owner}
   if MutexEntry.Owner = ThreadGetCurrent then
    begin
     {Update Count}
     Inc(MutexEntry.Count);
    end
   else
    begin
     {Acquire the Lock}
     while InterlockedExchange(LongInt(MutexEntry.State),MUTEX_STATE_LOCKED) <> MUTEX_STATE_UNLOCKED do
      begin
       while MutexEntry.State <> MUTEX_STATE_UNLOCKED do
        begin
         {Check Count}
         if Count < MutexEntry.SpinCount then
          begin
           {Increment Count}
           Inc(Count);
          end
         else
          begin
           {Yield while waiting}
           MutexEntry.Yield;
          end;

         {Check Signature}
         if MutexEntry.Signature <> MUTEX_SIGNATURE then Exit;
        end;
      end;

     {Memory Barrier}
     DataMemoryBarrier;

     {Set Count}
     MutexEntry.Count:=1;

     {Set Owner}
     MutexEntry.Owner:=ThreadGetCurrent;
    end;

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function MutexUnlockDefault(MutexEntry:PMutexEntry):LongWord;
{Default version of MutexUnlock function used if no handler is registered}
{Note: Not intended to be called directly by applications, use MutexUnlock instead}
begin
 {}
 {Check the Lock}
 if MutexEntry.State <> MUTEX_STATE_LOCKED then
  begin
   {$IFDEF MUTEX_DEBUG}
   InterlockedIncrement(LongInt(MutexUnlockNoLock));
   {$ENDIF}

   Result:=ERROR_NOT_LOCKED;
   Exit;
  end;

 {Check the Owner}
 if MutexEntry.Owner <> ThreadGetCurrent then
  begin
   {$IFDEF MUTEX_DEBUG}
   InterlockedIncrement(LongInt(MutexUnlockNoOwner));
   {$ENDIF}

   Result:=ERROR_NOT_OWNER;
   Exit;
  end;

 {Check the Flags}
 if (MutexEntry.Flags and MUTEX_FLAG_RECURSIVE) = 0 then
  begin
   {Release Owner}
   MutexEntry.Owner:=INVALID_HANDLE_VALUE;

   {Memory Barrier}
   DataMemoryBarrier;

   {Release the Lock}
   MutexEntry.State:=MUTEX_STATE_UNLOCKED;

   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Check Count}
   if MutexEntry.Count = 0 then
    begin
     Result:=ERROR_INVALID_FUNCTION;
     Exit;
    end;

   {Update Count}
   Dec(MutexEntry.Count);

   {Check Count}
   if MutexEntry.Count = 0 then
    begin
     {Release Owner}
     MutexEntry.Owner:=INVALID_HANDLE_VALUE;

     {Memory Barrier}
     DataMemoryBarrier;

     {Release the Lock}
     MutexEntry.State:=MUTEX_STATE_UNLOCKED;
    end;

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function MutexTryLockDefault(MutexEntry:PMutexEntry):LongWord;
{Default version of MutexTryLock function used if no handler is registered}
{Note: Not intended to be called directly by applications, use MutexTryLock instead}
begin
 {}
 {Check the Flags}
 if (MutexEntry.Flags and MUTEX_FLAG_RECURSIVE) = 0 then
  begin
   {Check the Lock}
   Result:=ERROR_LOCKED;
   if MutexEntry.State = MUTEX_STATE_LOCKED then Exit;

   {Acquire the Lock}
   if InterlockedExchange(LongInt(MutexEntry.State),MUTEX_STATE_LOCKED) = MUTEX_STATE_UNLOCKED then
    begin
     {Memory Barrier}
     DataMemoryBarrier;

     {Set Owner}
     MutexEntry.Owner:=ThreadGetCurrent;

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
  end
 else
  begin
   {Check Owner}
   if MutexEntry.Owner = ThreadGetCurrent then
    begin
     {Update Count}
     Inc(MutexEntry.Count);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end
   else
    begin
     {Check the Lock}
     Result:=ERROR_LOCKED;
     if MutexEntry.State = MUTEX_STATE_LOCKED then Exit;

     {Acquire the Lock}
     if InterlockedExchange(LongInt(MutexEntry.State),MUTEX_STATE_LOCKED) = MUTEX_STATE_UNLOCKED then
      begin
       {Memory Barrier}
       DataMemoryBarrier;

       {Set Count}
       MutexEntry.Count:=1;

       {Set Owner}
       MutexEntry.Owner:=ThreadGetCurrent;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Critical Section Functions}
function CriticalSectionCreate:TCriticalSectionHandle; {$IFDEF CRITICALSECTION_INLINE}inline;{$ENDIF}
{Create and insert a new CriticalSection entry}
{Return: Handle of new CriticalSection entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=CriticalSectionCreateEx(False,CRITICAL_SECTION_DEFAULT_SPINCOUNT);
end;

{==============================================================================}

function CriticalSectionCreateEx(InitialOwner:Boolean;SpinCount:LongWord):TCriticalSectionHandle;
{Create and insert a new CriticalSection entry}
{InitialOwner: If true set the state of the criticalsection to locked and the owner to the current thread}
{SpinCount: The number of times the criticalsection will spin before waiting (Always 0 if CPU count equals 1)}
{Return: Handle of new CriticalSection entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create CriticalSection entry}
 if CRITICAL_SECTION_SHARED_MEMORY then
  begin
   CriticalSectionEntry:=AllocSharedMem(SizeOf(TCriticalSectionEntry));
  end
 else
  begin
   CriticalSectionEntry:=AllocMem(SizeOf(TCriticalSectionEntry));
  end;
 if CriticalSectionEntry = nil then Exit;

 {Setup CriticalSection entry}
 CriticalSectionEntry.Signature:=CRITICAL_SECTION_SIGNATURE;
 CriticalSectionEntry.State:=CRITICAL_SECTION_STATE_UNLOCKED;
 CriticalSectionEntry.Count:=0;
 CriticalSectionEntry.Owner:=INVALID_HANDLE_VALUE;
 CriticalSectionEntry.SpinCount:=SpinCount;
 CriticalSectionEntry.Lock:=SpinCreate;
 CriticalSectionEntry.List:=INVALID_HANDLE_VALUE;
 CriticalSectionEntry.Wait:=ThreadWait;
 CriticalSectionEntry.WaitEx:=ThreadWaitEx;
 CriticalSectionEntry.Release:=ThreadRelease;
 CriticalSectionEntry.Abandon:=ThreadAbandon;

 {Setup CriticalSection entry}
 if InitialOwner then
  begin
   CriticalSectionEntry.State:=CRITICAL_SECTION_STATE_LOCKED;
   CriticalSectionEntry.Count:=1;
   CriticalSectionEntry.Owner:=ThreadGetCurrent;
  end;
 if SCHEDULER_CPU_COUNT = 1 then
  begin
   CriticalSectionEntry.SpinCount:=0;
  end;

 {Insert CriticalSection entry}
 if SpinLock(CriticalSectionTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link CriticalSection entry}
    if CriticalSectionTable = nil then
     begin
      CriticalSectionTable:=CriticalSectionEntry;
     end
    else
     begin
      CriticalSectionEntry.Next:=CriticalSectionTable;
      CriticalSectionTable.Prev:=CriticalSectionEntry;
      CriticalSectionTable:=CriticalSectionEntry;
     end;

    {Increment CriticalSection Count}
    Inc(CriticalSectionTableCount);

    {Return CriticalSection entry}
    Result:=TCriticalSectionHandle(CriticalSectionEntry);
   finally
    SpinUnlock(CriticalSectionTableLock);
   end;
  end
 else
  begin
   {Free CriticalSection Lock}
   SpinDestroy(CriticalSectionEntry.Lock);

   {Free CriticalSection Entry}
   FreeMem(CriticalSectionEntry);
  end;
end;

{==============================================================================}

function CriticalSectionDestroy(CriticalSection:TCriticalSectionHandle):LongWord;
{Destroy and remove an existing CriticalSection entry}
{CriticalSection: Handle of CriticalSection entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 CriticalSectionEntry:PCriticalSectionEntry;
 PrevEntry:PCriticalSectionEntry;
 NextEntry:PCriticalSectionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CriticalSection}
 if CriticalSection = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
 if CriticalSectionEntry = nil then Exit;
 if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

 {Acquire the CriticalSection}
 Result:=CriticalSectionLockEx(CriticalSection,INFINITE);
 if Result <> ERROR_SUCCESS then Exit;

 {Remove CriticalSection entry}
 if SpinLock(CriticalSectionTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=SpinLock(CriticalSectionEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then
     begin
      {Release the Lock}
      Result:=SpinUnlock(CriticalSectionEntry.Lock);
      if Result <> ERROR_SUCCESS then Exit;

      {Release the CriticalSection}
      Result:=CriticalSectionUnlock(CriticalSection);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate CriticalSection entry}
    CriticalSectionEntry.Signature:=0;

    {Check Waiting Threads}
    while ListNotEmpty(CriticalSectionEntry.List) do
     begin
      {Abandon waiting thread}
      CriticalSectionEntry.Abandon(CriticalSectionEntry.List);
     end;

    {Unlink CriticalSection entry}
    PrevEntry:=CriticalSectionEntry.Prev;
    NextEntry:=CriticalSectionEntry.Next;
    if PrevEntry = nil then
     begin
      CriticalSectionTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement CriticalSection Count}
    Dec(CriticalSectionTableCount);

    {Release the Lock}
    Result:=SpinUnlock(CriticalSectionEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Free CriticalSection List}
    if CriticalSectionEntry.List <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(CriticalSectionEntry.List);
     end;

    {Free CriticalSection Lock}
    SpinDestroy(CriticalSectionEntry.Lock);

    {Free CriticalSection Entry}
    FreeMem(CriticalSectionEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(CriticalSectionTableLock);
   end;
  end
 else
  begin
   {Release the CriticalSection}
   Result:=CriticalSectionUnlock(CriticalSection);
   if Result <> ERROR_SUCCESS then Exit;

   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CriticalSectionCount(CriticalSection:TCriticalSectionHandle):LongWord;
{Get the current lock count of an existing CriticalSection entry}
{CriticalSection: CriticalSection to get count for}
{Return: Current lock count or INVALID_HANDLE_VALUE on error}
var
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check CriticalSection}
 if CriticalSection = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
 if CriticalSectionEntry = nil then Exit;
 if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

 {Lock the CriticalSection}
 if SpinLock(CriticalSectionEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

    {Get the Count}
    Result:=CriticalSectionEntry.Count;
   finally
    {Unlock the CriticalSection}
    SpinUnlock(CriticalSectionEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function CriticalSectionOwner(CriticalSection:TCriticalSectionHandle):TThreadHandle;
{Get the current owner of an existing CriticalSection entry}
{CriticalSection: CriticalSection to get owner for}
{Return: Handle of owning thread or INVALID_HANDLE_VALUE if not currently owned}
var
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check CriticalSection}
 if CriticalSection = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
 if CriticalSectionEntry = nil then Exit;
 if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

 {Lock the CriticalSection}
 if SpinLock(CriticalSectionEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

    {Get the Owner}
    Result:=CriticalSectionEntry.Owner;
   finally
    {Unlock the CriticalSection}
    SpinUnlock(CriticalSectionEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function CriticalSectionSetSpinCount(CriticalSection:TCriticalSectionHandle;SpinCount:LongWord):LongWord;
{Set the spin count of an existing CriticalSection entry}
{CriticalSection: CriticalSection to set spin count for}
{SpinCount: The spin count value to set}
{Return: Current spin count or INVALID_HANDLE_VALUE on error}
var
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check CriticalSection}
 if CriticalSection = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
 if CriticalSectionEntry = nil then Exit;
 if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

 {Lock the CriticalSection}
 if SpinLock(CriticalSectionEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

    {Get the Spin Count}
    Result:=CriticalSectionEntry.SpinCount;

    {Set the Spin Count}
    CriticalSectionEntry.SpinCount:=SpinCount;
   finally
    {Unlock the CriticalSection}
    SpinUnlock(CriticalSectionEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function CriticalSectionLock(CriticalSection:TCriticalSectionHandle):LongWord;
{Lock an existing CriticalSection entry

 If the CriticalSection is not locked then lock it, set the count to one and
 mark the owner as the current thread

 If the CriticalSection is already locked by the current thread then increment
 the count and return immediately

 If the CriticalSection is already locked by another thread then wait until it
 is unlocked}
{CriticalSection: CriticalSection to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 {Check the Handler}
 if Assigned(CriticalSectionLockHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check CriticalSection}
   if CriticalSection = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
   if CriticalSectionEntry = nil then Exit;
   if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

   {$IFDEF LOCK_DEBUG}
   if SCHEDULER_FIQ_ENABLED then
    begin
     if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(CriticalSectionDeadlockCounter);
      end;
    end
   else
    begin
     if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(CriticalSectionDeadlockCounter);
      end;
    end;
   {$ENDIF}

   {Use the Handler method}
   Result:=CriticalSectionLockHandler(CriticalSectionEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=CriticalSectionLockEx(CriticalSection,INFINITE);
  end;
end;

{==============================================================================}

function CriticalSectionLockEx(CriticalSection:TCriticalSectionHandle;Timeout:LongWord):LongWord;
{Lock an existing CriticalSection entry

 If the CriticalSection is not locked then lock it, set the count to one and
 mark the owner as the current thread

 If the CriticalSection is already locked by the current thread then increment
 the count and return immediately

 If the CriticalSection is already locked by another thread then wait until it
 is unlocked}
{CriticalSection: CriticalSection to lock}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Unlock:Boolean;
 WaitResult:LongWord;
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CriticalSection}
 if CriticalSection = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
 if CriticalSectionEntry = nil then Exit;
 if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(CriticalSectionDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(CriticalSectionDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(CriticalSectionLockExHandler) then
  begin
   {Use the Handler method}
   Result:=CriticalSectionLockExHandler(CriticalSectionEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Check State and Owner}
   if (CriticalSectionEntry.State = CRITICAL_SECTION_STATE_LOCKED) and (CriticalSectionEntry.Owner = ThreadGetCurrent) then
    begin
     {Update Count}
     Inc(CriticalSectionEntry.Count);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end
   else
    begin
     {Lock the CriticalSection}
     if SpinLock(CriticalSectionEntry.Lock) = ERROR_SUCCESS then
      begin
       Unlock:=True;
       try
        {Check Signature}
        if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

        {Check Timeout}
        if Timeout = 0 then
         begin
          {Check State}
          Result:=ERROR_WAIT_TIMEOUT;
          if (CriticalSectionEntry.State <> CRITICAL_SECTION_STATE_UNLOCKED) and (CriticalSectionEntry.Owner <> ThreadGetCurrent) then Exit;
         end;

        {Check State}
        if CriticalSectionEntry.State = CRITICAL_SECTION_STATE_UNLOCKED then
         begin
          {Set State}
          CriticalSectionEntry.State:=CRITICAL_SECTION_STATE_LOCKED;

          {Set Count}
          CriticalSectionEntry.Count:=1;

          {Set Owner}
          CriticalSectionEntry.Owner:=ThreadGetCurrent;
         end
        else
         begin
          {Check Owner}
          if CriticalSectionEntry.Owner = ThreadGetCurrent then
           begin
            {Update Count}
            Inc(CriticalSectionEntry.Count);
           end
          else
           begin
            {Check Spin Count}
            if CriticalSectionEntry.SpinCount > 0 then
             begin
              Count:=0;

              {Check Count and State}
              while (Count < CriticalSectionEntry.SpinCount) and (CriticalSectionEntry.State <> CRITICAL_SECTION_STATE_UNLOCKED) do
               begin
                {Unlock the CriticalSection}
                SpinUnlock(CriticalSectionEntry.Lock);

                {Increment Count}
                Inc(Count);

                {Lock the CriticalSection}
                if SpinLock(CriticalSectionEntry.Lock) <> ERROR_SUCCESS then Exit;

                {Check Signature}
                if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;
               end;

              {Check State}
              if CriticalSectionEntry.State = CRITICAL_SECTION_STATE_UNLOCKED then
               begin
                {Set State}
                CriticalSectionEntry.State:=CRITICAL_SECTION_STATE_LOCKED;

                {Set Count}
                CriticalSectionEntry.Count:=1;

                {Set Owner}
                CriticalSectionEntry.Owner:=ThreadGetCurrent;

                {Return Result}
                Result:=ERROR_SUCCESS;
                Exit;
               end;
             end;

            {Check List}
            if CriticalSectionEntry.List = INVALID_HANDLE_VALUE then
             begin
              {Create List}
              CriticalSectionEntry.List:=ListCreateEx(LIST_TYPE_WAIT_SECTION,SchedulerGetListFlags(LIST_TYPE_WAIT_SECTION));
             end;

            {Check Timeout}
            if Timeout = INFINITE then
             begin
              {Wait on CriticalSection}
              CriticalSectionEntry.Wait(CriticalSectionEntry.List,CriticalSectionEntry.Lock,LOCK_FLAG_NONE);
              Unlock:=False;

              {Check Result}
              WaitResult:=ThreadGetWaitResult;
              if WaitResult = WAIT_TIMEOUT then
               begin
                Result:=ERROR_WAIT_TIMEOUT;
                Exit;
               end
              else if WaitResult = WAIT_ABANDONED then
               begin
                Result:=ERROR_WAIT_ABANDONED;
                Exit;
               end
              else if WaitResult <> ERROR_SUCCESS then
               begin
                Result:=WaitResult;
                Exit;
               end;

              {Lock CriticalSection (Infinite Wait)}
              Result:=CriticalSectionLockEx(CriticalSection,INFINITE);
              Exit;
             end
            else
             begin
              {Wait on CriticalSection with Timeout}
              CriticalSectionEntry.WaitEx(CriticalSectionEntry.List,CriticalSectionEntry.Lock,LOCK_FLAG_NONE,Timeout);
              Unlock:=False;

              {Check Result}
              WaitResult:=ThreadGetWaitResult;
              if WaitResult = WAIT_TIMEOUT then
               begin
                Result:=ERROR_WAIT_TIMEOUT;
                Exit;
               end
              else if WaitResult = WAIT_ABANDONED then
               begin
                Result:=ERROR_WAIT_ABANDONED;
                Exit;
               end
              else if WaitResult <> ERROR_SUCCESS then
               begin
                Result:=WaitResult;
                Exit;
               end;

              {Lock CriticalSection (No Wait)}
              Result:=CriticalSectionLockEx(CriticalSection,0);
              Exit;
             end;
           end;
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       finally
        {Unlock the CriticalSection}
        if Unlock then SpinUnlock(CriticalSectionEntry.Lock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
      end;
    end;
  end;
end;

{==============================================================================}

function CriticalSectionUnlock(CriticalSection:TCriticalSectionHandle):LongWord;
{Unlock an existing CriticalSection entry

 If the CriticalSection is locked by the current thread then decrement the count

 If the count is greater than zero then return immediately

 If the count reaches zero then unlock the CriticalSection and release the first
 thread waiting for it to be unlocked

 If the CriticalSection is locked by another thread then return an error

 If the CriticalSection is not locked then return an error}
{CriticalSection: CriticalSection to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CriticalSection}
 if CriticalSection = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
 if CriticalSectionEntry = nil then Exit;
 if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

 {Check the Lock}
 Result:=ERROR_NOT_LOCKED;
 if CriticalSectionEntry.State <> CRITICAL_SECTION_STATE_LOCKED then Exit;

 {Check the Owner}
 Result:=ERROR_NOT_OWNER;
 if CriticalSectionEntry.Owner <> ThreadGetCurrent then Exit;

 {Check the Handler}
 if Assigned(CriticalSectionUnlockHandler) then
  begin
   {Use the Handler method}
   Result:=CriticalSectionUnlockHandler(CriticalSectionEntry);
  end
 else
  begin
   {Use the Default method}
   {Check Count}
   if CriticalSectionEntry.Count > 1 then
    begin
     {Update Count}
     Dec(CriticalSectionEntry.Count);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end
   else
    begin
     {Lock the CriticalSection}
     if SpinLock(CriticalSectionEntry.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Signature}
        Result:=ERROR_INVALID_PARAMETER;
        if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

        {Check Count}
        Result:=ERROR_INVALID_FUNCTION;
        if CriticalSectionEntry.Count = 0 then Exit;

        {Update Count}
        Dec(CriticalSectionEntry.Count);

        {Check Count}
        if CriticalSectionEntry.Count = 0 then
         begin
          {Set State}
          CriticalSectionEntry.State:=CRITICAL_SECTION_STATE_UNLOCKED;

          {Set Owner}
          CriticalSectionEntry.Owner:=INVALID_HANDLE_VALUE;

          {Check List}
          while ListNotEmpty(CriticalSectionEntry.List) do
           begin
            {Release one thread waiting on CriticalSection}
            if CriticalSectionEntry.Release(CriticalSectionEntry.List) = ERROR_SUCCESS then
             begin
              Break;
             end;
           end;
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       finally
        {Unlock the CriticalSection}
        SpinUnlock(CriticalSectionEntry.Lock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
      end;
    end;
  end;
end;

{==============================================================================}

function CriticalSectionTryLock(CriticalSection:TCriticalSectionHandle):LongWord;
{Try to lock an existing CriticalSection entry

 If the CriticalSection is not locked then lock it, set the count to one and
 mark the owner as the current thread

 If the CriticalSection is already locked by the current thread then increment
 the count and return immediately

 If the CriticalSection is already locked by another thread then return immediately
 with an error and do not wait for it to be unlocked}
{CriticalSection: CriticalSection to try to lock}
{Return: ERROR_SUCCESS if completed, ERROR_LOCKED if locked by another thread or another error code on failure}
var
 CriticalSectionEntry:PCriticalSectionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CriticalSection}
 if CriticalSection = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CriticalSectionEntry:=PCriticalSectionEntry(CriticalSection);
 if CriticalSectionEntry = nil then Exit;
 if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(CriticalSectionTryLockHandler) then
  begin
   {Use the Handler method}
   Result:=CriticalSectionTryLockHandler(CriticalSectionEntry);
  end
 else
  begin
   {Use the Default method}
   {Check State and Owner}
   if (CriticalSectionEntry.State = CRITICAL_SECTION_STATE_LOCKED) and (CriticalSectionEntry.Owner = ThreadGetCurrent) then
    begin
     {Update Count}
     Inc(CriticalSectionEntry.Count);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end
   else
    begin
     {Lock the CriticalSection}
     if SpinLock(CriticalSectionEntry.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Signature}
        if CriticalSectionEntry.Signature <> CRITICAL_SECTION_SIGNATURE then Exit;

        {Check State}
        if CriticalSectionEntry.State = CRITICAL_SECTION_STATE_UNLOCKED then
         begin
          {Set State}
          CriticalSectionEntry.State:=CRITICAL_SECTION_STATE_LOCKED;

          {Set Count}
          CriticalSectionEntry.Count:=1;

          {Set Owner}
          CriticalSectionEntry.Owner:=ThreadGetCurrent;
         end
        else
         begin
          {Check Owner}
          if CriticalSectionEntry.Owner = ThreadGetCurrent then
           begin
            {Update Count}
            Inc(CriticalSectionEntry.Count);
           end
          else
           begin
            Result:=ERROR_LOCKED;
            Exit;
           end;
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       finally
        {Unlock the CriticalSection}
        SpinUnlock(CriticalSectionEntry.Lock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Semaphore Functions}
function SemaphoreCreate(Count:LongWord):TSemaphoreHandle; {$IFDEF SEMAPHORE_INLINE}inline;{$ENDIF}
{Create and insert a new Semaphore entry}
{Count: The initial count of the Semaphore (Must be greater than or equal to zero)}
{Return: Handle of new Semaphore entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=SemaphoreCreateEx(Count,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_NONE);
end;

{==============================================================================}

function SemaphoreCreateEx(Count,Maximum:LongWord;Flags:LongWord):TSemaphoreHandle;
{Create and insert a new Semaphore entry}
{Count: The initial count of the Semaphore (Must be greater than or equal to zero)}
{Maximum: The maximum count of the Semaphore (Must be greater than one)}
{Flags: The flags for the Semaphore entry (eg SEMAPHORE_FLAG_IRQ)}
{Return: Handle of new Semaphore entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 SemaphoreEntry:PSemaphoreEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Count}
 {if Count < 0 then Exit;}
 if Count > Maximum then Exit;

 {Check Maximum}
 if Maximum < 1 then Exit;

 {Create Semaphore entry}
 if SEMAPHORE_SHARED_MEMORY then
  begin
   SemaphoreEntry:=AllocSharedMem(SizeOf(TSemaphoreEntry));
  end
 else
  begin
   SemaphoreEntry:=AllocMem(SizeOf(TSemaphoreEntry));
  end;
 if SemaphoreEntry = nil then Exit;

 {Setup Semaphore entry}
 SemaphoreEntry.Signature:=SEMAPHORE_SIGNATURE;
 SemaphoreEntry.Count:=Count;
 SemaphoreEntry.Maximum:=Maximum;
 SemaphoreEntry.Flags:=Flags;
 SemaphoreEntry.Lock:=SpinCreate;
 SemaphoreEntry.List:=INVALID_HANDLE_VALUE;
 SemaphoreEntry.Wait:=ThreadWait;
 SemaphoreEntry.WaitEx:=ThreadWaitEx;
 SemaphoreEntry.Release:=ThreadRelease;
 SemaphoreEntry.Abandon:=ThreadAbandon;

 {Check Semaphore flags}
 if (Flags and (SEMAPHORE_FLAG_IRQ or SEMAPHORE_FLAG_FIQ or SEMAPHORE_FLAG_IRQFIQ)) <> 0 then
  begin
   {Create Semaphore List}
   SemaphoreEntry.List:=ListCreateEx(LIST_TYPE_WAIT_SEMAPHORE,SchedulerGetListFlags(LIST_TYPE_WAIT_SEMAPHORE));
  end;

 {Insert Semaphore entry}
 if SpinLock(SemaphoreTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Semaphore entry}
    if SemaphoreTable = nil then
     begin
      SemaphoreTable:=SemaphoreEntry;
     end
    else
     begin
      SemaphoreEntry.Next:=SemaphoreTable;
      SemaphoreTable.Prev:=SemaphoreEntry;
      SemaphoreTable:=SemaphoreEntry;
     end;

    {Increment Semaphore Count}
    Inc(SemaphoreTableCount);

    {Return Semaphore entry}
    Result:=TSemaphoreHandle(SemaphoreEntry);
   finally
    SpinUnlock(SemaphoreTableLock);
   end;
  end
 else
  begin
   {Free Semaphore List}
   if SemaphoreEntry.List <> INVALID_HANDLE_VALUE then
    begin
     ListDestroy(SemaphoreEntry.List);
    end;

   {Free Semaphore Lock}
   SpinDestroy(SemaphoreEntry.Lock);

   {Free Semaphore Entry}
   FreeMem(SemaphoreEntry);
  end;
end;

{==============================================================================}

function SemaphoreDestroy(Semaphore:TSemaphoreHandle):LongWord;
{Destroy and remove an existing Semaphore entry}
{Semaphore: Handle of Semaphore entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SemaphoreEntry:PSemaphoreEntry;
 PrevEntry:PSemaphoreEntry;
 NextEntry:PSemaphoreEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Semaphore}
 if Semaphore = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SemaphoreEntry:=PSemaphoreEntry(Semaphore);
 if SemaphoreEntry = nil then Exit;
 if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

 {Remove Semaphore entry}
 if SpinLock(SemaphoreTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinLockIRQFIQ(SemaphoreEntry.Lock);
     end
    else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinLockFIQ(SemaphoreEntry.Lock);
     end
    else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinLockIRQ(SemaphoreEntry.Lock);
     end
    else
     begin
      Result:=SpinLock(SemaphoreEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then
     begin
      {Release the Lock}
      if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
       begin
        Result:=SpinUnlockIRQFIQ(SemaphoreEntry.Lock);
       end
      else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
       begin
        Result:=SpinUnlockFIQ(SemaphoreEntry.Lock);
       end
      else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
       begin
        Result:=SpinUnlockIRQ(SemaphoreEntry.Lock);
       end
      else
       begin
        Result:=SpinUnlock(SemaphoreEntry.Lock);
       end;
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Semaphore entry}
    SemaphoreEntry.Signature:=0;

    {Check Waiting Threads}
    while ListNotEmpty(SemaphoreEntry.List) do
     begin
      {Abandon waiting thread}
      SemaphoreEntry.Abandon(SemaphoreEntry.List);
     end;

    {Unlink Semaphore entry}
    PrevEntry:=SemaphoreEntry.Prev;
    NextEntry:=SemaphoreEntry.Next;
    if PrevEntry = nil then
     begin
      SemaphoreTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Semaphore Count}
    Dec(SemaphoreTableCount);

    {Release the Lock}
    if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinUnlockIRQFIQ(SemaphoreEntry.Lock);
     end
    else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinUnlockFIQ(SemaphoreEntry.Lock);
     end
    else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinUnlockIRQ(SemaphoreEntry.Lock);
     end
    else
     begin
      Result:=SpinUnlock(SemaphoreEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Free Semaphore List}
    if SemaphoreEntry.List <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(SemaphoreEntry.List);
     end;

    {Free Semaphore Lock}
    SpinDestroy(SemaphoreEntry.Lock);

    {Free Semaphore Entry}
    FreeMem(SemaphoreEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(SemaphoreTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SemaphoreCount(Semaphore:TSemaphoreHandle):LongWord;
{Get the current count of an existing Semaphore entry}
{Semaphore: Semaphore to get count for}
{Return: Current count or INVALID_HANDLE_VALUE on error}
var
 ResultCode:LongWord;
 SemaphoreEntry:PSemaphoreEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Semaphore}
 if Semaphore = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SemaphoreEntry:=PSemaphoreEntry(Semaphore);
 if SemaphoreEntry = nil then Exit;
 if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

 {Lock the Semaphore}
 if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQFIQ(SemaphoreEntry.Lock);
  end
 else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
  begin
   ResultCode:=SpinLockFIQ(SemaphoreEntry.Lock);
  end
 else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQ(SemaphoreEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLock(SemaphoreEntry.Lock);
  end;
 {Check Lock Result}
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

    {Get the Count}
    Result:=SemaphoreEntry.Count;
   finally
    {Unlock the Semaphore}
    if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
     begin
      SpinUnlockIRQFIQ(SemaphoreEntry.Lock);
     end
    else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
     begin
      SpinUnlockFIQ(SemaphoreEntry.Lock);
     end
    else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
     begin
      SpinUnlockIRQ(SemaphoreEntry.Lock);
     end
    else
     begin
      SpinUnlock(SemaphoreEntry.Lock);
     end;
   end;
  end;
end;

{==============================================================================}

function SemaphoreWait(Semaphore:TSemaphoreHandle):LongWord;
{Wait on an existing Semaphore entry

 If the Semaphore count is greater than zero it will be decremented
 and this function will return immediately

 If the Semaphore count is zero the current thread will be put on a wait queue
 until the Semaphore is signalled by another thread calling SemaphoreSignal()
 or SemaphoreSignalEx()}
{Semaphore: Semaphore to wait on}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SemaphoreEntry:PSemaphoreEntry;
begin
 {}
 {Check the Handler}
 if Assigned(SemaphoreWaitHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check Semaphore}
   if Semaphore = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   SemaphoreEntry:=PSemaphoreEntry(Semaphore);
   if SemaphoreEntry = nil then Exit;
   if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

   {Use the Handler method}
   Result:=SemaphoreWaitHandler(SemaphoreEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=SemaphoreWaitEx(Semaphore,INFINITE);
  end;
end;

{==============================================================================}

function SemaphoreWaitEx(Semaphore:TSemaphoreHandle;Timeout:LongWord):LongWord;
{Wait on an existing Semaphore entry

 If the Semaphore count is greater than zero it will be decremented
 and this function will return immediately

 If the Semaphore count is zero the current thread will be put on a wait queue
 until the Semaphore is signalled by another thread calling SemaphoreSignal()
 or SemaphoreSignalEx()}
{Semaphore: Semaphore to wait on}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 ResultCode:LongWord;
 WaitResult:LongWord;
 SemaphoreEntry:PSemaphoreEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Semaphore}
 if Semaphore = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SemaphoreEntry:=PSemaphoreEntry(Semaphore);
 if SemaphoreEntry = nil then Exit;
 if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(SemaphoreWaitExHandler) then
  begin
   {Use the Handler method}
   Result:=SemaphoreWaitExHandler(SemaphoreEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Semaphore}
   if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQFIQ(SemaphoreEntry.Lock);
    end
   else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
    begin
     ResultCode:=SpinLockFIQ(SemaphoreEntry.Lock);
    end
   else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQ(SemaphoreEntry.Lock);
    end
   else
    begin
     {$IFDEF LOCK_DEBUG}
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(SemaphoreDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(SemaphoreDeadlockCounter);
        end;
      end;
     {$ENDIF}

     ResultCode:=SpinLock(SemaphoreEntry.Lock);
    end;
   {Check Lock Result}
   if ResultCode = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check Count}
        Result:=ERROR_WAIT_TIMEOUT;
        if SemaphoreEntry.Count = 0 then Exit;
       end;

      {Check Count}
      if SemaphoreEntry.Count > 0 then
       begin
        {Decrement Semaphore}
        Dec(SemaphoreEntry.Count);
       end
      else
       begin
        {Check List}
        if SemaphoreEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          SemaphoreEntry.List:=ListCreateEx(LIST_TYPE_WAIT_SEMAPHORE,SchedulerGetListFlags(LIST_TYPE_WAIT_SEMAPHORE));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Semaphore}
          SemaphoreEntry.Wait(SemaphoreEntry.List,SemaphoreEntry.Lock,SemaphoreEntry.Flags);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end
        else
         begin
          {Wait on Semaphore with Timeout}
          SemaphoreEntry.WaitEx(SemaphoreEntry.List,SemaphoreEntry.Lock,SemaphoreEntry.Flags,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Semaphore}
      if Unlock then
       begin
        if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
         begin
          SpinUnlockIRQFIQ(SemaphoreEntry.Lock);
         end
        else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
         begin
          SpinUnlockFIQ(SemaphoreEntry.Lock);
         end
        else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
         begin
          SpinUnlockIRQ(SemaphoreEntry.Lock);
         end
        else
         begin
          SpinUnlock(SemaphoreEntry.Lock);
         end;
       end;
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function SemaphoreSignal(Semaphore:TSemaphoreHandle):LongWord;
{Signal an existing Semaphore entry

 If any threads are waiting on the Semaphore then one thread will be woken up and
 placed on the ready queue

 If no threads are waiting then the Semaphore count will be incremented by one}
{Semaphore: Semaphore to signal}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=SemaphoreSignalEx(Semaphore,1,nil);
end;

{==============================================================================}

function SemaphoreSignalEx(Semaphore:TSemaphoreHandle;Count:LongWord;Previous:PLongWord):LongWord;
{Signal an existing Semaphore entry one or more times

 If any threads are waiting on the Semaphore then one thread will be woken up and
 placed on the ready queue for each iteration of the count passed

 If no threads are waiting then the Semaphore count will be incremented once for each
 iteration of the count passed}
{Semaphore: Semaphore to signal}
{Count: The number is times to signal the Semaphore, must be greater than zero}
{Previous: A pointer to a value that receives the previous count of the Semaphore
           Can be nil if the previous count is not required}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Current:LongWord;
 ResultCode:LongWord;
 SemaphoreEntry:PSemaphoreEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Count}
 if Count < 1 then Exit;

 {Check Semaphore}
 if Semaphore = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SemaphoreEntry:=PSemaphoreEntry(Semaphore);
 if SemaphoreEntry = nil then Exit;
 if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(SemaphoreSignalHandler) then
  begin
   {Use the Handler method}
   Current:=SemaphoreSignalHandler(SemaphoreEntry);
   Dec(Count);
   while Count > 0 do
    begin
     SemaphoreSignalHandler(SemaphoreEntry);
     Dec(Count);
    end;

   {Return Previous}
   if Previous <> nil then
    begin
     Previous^:=Current;
    end;

   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Use the Default method}
   {Lock the Semaphore}
   if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQFIQ(SemaphoreEntry.Lock);
    end
   else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
    begin
     ResultCode:=SpinLockFIQ(SemaphoreEntry.Lock);
    end
   else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQ(SemaphoreEntry.Lock);
    end
   else
    begin
     {$IFDEF LOCK_DEBUG}
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(SemaphoreDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(SemaphoreDeadlockCounter);
        end;
      end;
     {$ENDIF}

     ResultCode:=SpinLock(SemaphoreEntry.Lock);
    end;
   {Check Lock Result}
   if ResultCode = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if SemaphoreEntry.Signature <> SEMAPHORE_SIGNATURE then Exit;

      {Get Current Count}
      Current:=SemaphoreEntry.Count;
      while Count > 0 do
       begin
        if ListIsEmpty(SemaphoreEntry.List) then
         begin
          {Check Maximum}
          if SemaphoreEntry.Count < SemaphoreEntry.Maximum then
           begin
            {Increment Semaphore}
            Inc(SemaphoreEntry.Count);
           end;

          {Decrement Count}
          Dec(Count);
         end
        else
         begin
          {Release one thread waiting on Semaphore}
          if SemaphoreEntry.Release(SemaphoreEntry.List) = ERROR_SUCCESS then
           begin
            {Decrement Count}
            Dec(Count);
           end;
         end;
       end;

      {Return Previous}
      if Previous <> nil then
       begin
        Previous^:=Current;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Semaphore}
      if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQFIQ) <> 0 then
       begin
        SpinUnlockIRQFIQ(SemaphoreEntry.Lock);
       end
      else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_FIQ) <> 0 then
       begin
        SpinUnlockFIQ(SemaphoreEntry.Lock);
       end
      else if (SemaphoreEntry.Flags and SEMAPHORE_FLAG_IRQ) <> 0 then
       begin
        SpinUnlockIRQ(SemaphoreEntry.Lock);
       end
      else
       begin
        SpinUnlock(SemaphoreEntry.Lock);
       end;
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Synchronizer Functions}
function SynchronizerCreate:TSynchronizerHandle; {$IFDEF SYNCHRONIZER_INLINE}inline;{$ENDIF}
{Create and insert a new Synchronizer entry}
{Return: Handle of new Synchronizer entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=SynchronizerCreateEx(False,False);
end;

{==============================================================================}

function SynchronizerCreateEx(InitialReader,InitialWriter:Boolean):TSynchronizerHandle;
{Create and insert a new Synchronizer entry}
{InitialReader: If true set the state of the synchronizer to locked and the reader count to 1}
{InitialWriter: If true set the state of the synchronizer to locked and the writer owner to the current thread}
{Return: Handle of new Synchronizer entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create Synchronizer entry}
 if SYNCHRONIZER_SHARED_MEMORY then
  begin
   SynchronizerEntry:=AllocSharedMem(SizeOf(TSynchronizerEntry));
  end
 else
  begin
   SynchronizerEntry:=AllocMem(SizeOf(TSynchronizerEntry));
  end;
 if SynchronizerEntry = nil then Exit;

 {Setup Synchronizer entry}
 SynchronizerEntry.Signature:=SYNCHRONIZER_SIGNATURE;
 SynchronizerEntry.State:=SYNCHRONIZER_STATE_UNLOCKED;
 SynchronizerEntry.Lock:=SpinCreate;
 SynchronizerEntry.ReaderCount:=0;
 SynchronizerEntry.WriterCount:=0;
 SynchronizerEntry.ReaderLast:=INVALID_HANDLE_VALUE;
 SynchronizerEntry.WriterOwner:=INVALID_HANDLE_VALUE;
 SynchronizerEntry.ReaderList:=INVALID_HANDLE_VALUE;
 SynchronizerEntry.WriterList:=INVALID_HANDLE_VALUE;
 SynchronizerEntry.Wait:=ThreadWait;
 SynchronizerEntry.WaitEx:=ThreadWaitEx;
 SynchronizerEntry.Release:=ThreadRelease;
 SynchronizerEntry.Abandon:=ThreadAbandon;

 {Setup Synchronizer entry}
 if InitialWriter then
  begin
   SynchronizerEntry.State:=SYNCHRONIZER_STATE_WRITER_LOCKED;
   SynchronizerEntry.WriterCount:=1;
   SynchronizerEntry.WriterOwner:=ThreadGetCurrent;
  end
 else
  begin
   if InitialReader then
    begin
     SynchronizerEntry.State:=SYNCHRONIZER_STATE_READER_LOCKED;
     SynchronizerEntry.ReaderCount:=1;
     SynchronizerEntry.ReaderLast:=ThreadGetCurrent;
    end;
  end;

 {Insert Synchronizer entry}
 if SpinLock(SynchronizerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Synchronizer entry}
    if SynchronizerTable = nil then
     begin
      SynchronizerTable:=SynchronizerEntry;
     end
    else
     begin
      SynchronizerEntry.Next:=SynchronizerTable;
      SynchronizerTable.Prev:=SynchronizerEntry;
      SynchronizerTable:=SynchronizerEntry;
     end;

    {Increment Synchronizer Count}
    Inc(SynchronizerTableCount);

    {Return Synchronizer entry}
    Result:=TSynchronizerHandle(SynchronizerEntry);
   finally
    SpinUnlock(SynchronizerTableLock);
   end;
  end
 else
  begin
   {Free Synchronizer Lock}
   SpinDestroy(SynchronizerEntry.Lock);

   {Free Synchronizer Entry}
   FreeMem(SynchronizerEntry);
  end;
end;

{==============================================================================}

function SynchronizerDestroy(Synchronizer:TSynchronizerHandle):LongWord;
{Destroy and remove an existing Synchronizer entry}
{Synchronizer: Handle of Synchronizer entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SynchronizerEntry:PSynchronizerEntry;
 PrevEntry:PSynchronizerEntry;
 NextEntry:PSynchronizerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Acquire the Synchronizer}
 Result:=SynchronizerWriterLockEx(Synchronizer,INFINITE);
 if Result <> ERROR_SUCCESS then Exit;

 {Remove Synchronizer entry}
 if SpinLock(SynchronizerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=SpinLock(SynchronizerEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then
     begin
      {Release the Lock}
      Result:=SpinUnlock(SynchronizerEntry.Lock);
      if Result <> ERROR_SUCCESS then Exit;

      {Release the Synchronizer}
      Result:=SynchronizerWriterUnlock(Synchronizer);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Synchronizer entry}
    SynchronizerEntry.Signature:=0;

    {Check Waiting Writer Threads}
    while ListNotEmpty(SynchronizerEntry.WriterList) do
     begin
      {Abandon waiting thread}
      SynchronizerEntry.Abandon(SynchronizerEntry.WriterList);
     end;

    {Check Waiting Reader Threads}
    while ListNotEmpty(SynchronizerEntry.ReaderList) do
     begin
      {Abandon waiting thread}
      SynchronizerEntry.Abandon(SynchronizerEntry.ReaderList);
     end;

    {Unlink Synchronizer entry}
    PrevEntry:=SynchronizerEntry.Prev;
    NextEntry:=SynchronizerEntry.Next;
    if PrevEntry = nil then
     begin
      SynchronizerTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Synchronizer Count}
    Dec(SynchronizerTableCount);

    {Release the Lock}
    Result:=SpinUnlock(SynchronizerEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Free Synchronizer Writer List}
    if SynchronizerEntry.WriterList <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(SynchronizerEntry.WriterList);
     end;

    {Free Synchronizer Reader List}
    if SynchronizerEntry.ReaderList <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(SynchronizerEntry.ReaderList);
     end;

    {Free Synchronizer Lock}
    SpinDestroy(SynchronizerEntry.Lock);

    {Free Synchronizer Entry}
    FreeMem(SynchronizerEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(SynchronizerTableLock);
   end;
  end
 else
  begin
   {Release the Synchronizer}
   Result:=SynchronizerWriterUnlock(Synchronizer);
   if Result <> ERROR_SUCCESS then Exit;

   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SynchronizerReaderCount(Synchronizer:TSynchronizerHandle):LongWord;
{Get the current reader count of an existing Synchronizer entry}
{Synchronizer: Synchronizer to get count for}
{Return: Current reader count or INVALID_HANDLE_VALUE on error}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Lock the Synchronizer}
 if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

    {Get Reader Count}
    Result:=SynchronizerEntry.ReaderCount;
   finally
    {Unlock the Synchronizer}
    SpinUnlock(SynchronizerEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function SynchronizerReaderLast(Synchronizer:TSynchronizerHandle):TThreadHandle;
{Get the last reader thread of an existing Synchronizer entry}
{Synchronizer: Synchronizer to last reader for}
{Return: Last reader thread or INVALID_HANDLE_VALUE on error}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Lock the Synchronizer}
 if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

    {Get Reader Last}
    Result:=SynchronizerEntry.ReaderLast;
   finally
    {Unlock the Synchronizer}
    SpinUnlock(SynchronizerEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function SynchronizerReaderLock(Synchronizer:TSynchronizerHandle):LongWord;
{Lock an existing Synchronizer entry for reading

 If the Synchronizer is not locked then lock it and set the reader count to one

 If the Synchronizer is already locked for reading then increment the reader count
 and return immediately

 If the Synchronizer is already locked for writing then wait until it is unlocked}
{Synchronizer: Synchronizer to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 {Check the Handler}
 if Assigned(SynchronizerReaderLockHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check Synchronizer}
   if Synchronizer = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
   if SynchronizerEntry = nil then Exit;
   if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

   {$IFDEF LOCK_DEBUG}
   if SCHEDULER_FIQ_ENABLED then
    begin
     if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(SynchronizerDeadlockCounter);
      end;
    end
   else
    begin
     if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(SynchronizerDeadlockCounter);
      end;
    end;
   if (SynchronizerEntry.WriterOwner = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SynchronizerRecursionCounter);
    end;
   {$ENDIF}

   {Use the Handler method}
   Result:=SynchronizerReaderLockHandler(SynchronizerEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=SynchronizerReaderLockEx(Synchronizer,INFINITE);
  end;
end;

{==============================================================================}

function SynchronizerReaderLockEx(Synchronizer:TSynchronizerHandle;Timeout:LongWord):LongWord;
{Lock an existing Synchronizer entry for reading

 If the Synchronizer is not locked then lock it and set the reader count to one

 If the Synchronizer is already locked for reading then increment the reader count
 and return immediately

 If the Synchronizer is already locked for writing then wait until it is unlocked}
{Synchronizer: Synchronizer to lock}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SynchronizerDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SynchronizerDeadlockCounter);
    end;
  end;
 if (SynchronizerEntry.WriterOwner = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SynchronizerRecursionCounter);
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(SynchronizerReaderLockExHandler) then
  begin
   {Use the Handler method}
   Result:=SynchronizerReaderLockExHandler(SynchronizerEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Note: Cannot shortcut ReaderLock on Synchronizer}
   {Lock the Synchronizer}
   if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check Writer State}
        Result:=ERROR_WAIT_TIMEOUT;
        if SynchronizerEntry.WriterCount <> 0 then Exit;
        if SynchronizerEntry.WriterOwner <> INVALID_HANDLE_VALUE then Exit;
       end;

      {Check State}
      if SynchronizerEntry.State = SYNCHRONIZER_STATE_UNLOCKED then
       begin
        {Set State}
        SynchronizerEntry.State:=SYNCHRONIZER_STATE_READER_LOCKED;

        {Set Reader Count}
        SynchronizerEntry.ReaderCount:=1;

        {Set Reader Last}
        SynchronizerEntry.ReaderLast:=ThreadGetCurrent;
       end
      else
       begin
        {Check Writer Owner}
        if SynchronizerEntry.WriterOwner = INVALID_HANDLE_VALUE then
         begin
          {Update Reader Count}
          Inc(SynchronizerEntry.ReaderCount);

          {Update Reader Last}
          SynchronizerEntry.ReaderLast:=ThreadGetCurrent;
         end
        else
         begin
          {Check Reader List}
          if SynchronizerEntry.ReaderList = INVALID_HANDLE_VALUE then
           begin
            {Create Reader List}
            SynchronizerEntry.ReaderList:=ListCreateEx(LIST_TYPE_WAIT_SYNCHRONIZER,SchedulerGetListFlags(LIST_TYPE_WAIT_SYNCHRONIZER));
           end;

          {Check Timeout}
          if Timeout = INFINITE then
           begin
            {Wait on Synchronizer}
            SynchronizerEntry.Wait(SynchronizerEntry.ReaderList,SynchronizerEntry.Lock,LOCK_FLAG_NONE);
            Unlock:=False;

            {Check Result}
            WaitResult:=ThreadGetWaitResult;
            if WaitResult = WAIT_TIMEOUT then
             begin
              Result:=ERROR_WAIT_TIMEOUT;
              Exit;
             end
            else if WaitResult = WAIT_ABANDONED then
             begin
              Result:=ERROR_WAIT_ABANDONED;
              Exit;
             end
            else if WaitResult <> ERROR_SUCCESS then
             begin
              Result:=WaitResult;
              Exit;
             end;

            {Lock Synchronizer (Infinite Wait)}
            Result:=SynchronizerReaderLockEx(Synchronizer,INFINITE);
            Exit;
           end
          else
           begin
            {Wait on Synchronizer with Timeout}
            SynchronizerEntry.WaitEx(SynchronizerEntry.ReaderList,SynchronizerEntry.Lock,LOCK_FLAG_NONE,Timeout);
            Unlock:=False;

            {Check Result}
            WaitResult:=ThreadGetWaitResult;
            if WaitResult = WAIT_TIMEOUT then
             begin
              Result:=ERROR_WAIT_TIMEOUT;
              Exit;
             end
            else if WaitResult = WAIT_ABANDONED then
             begin
              Result:=ERROR_WAIT_ABANDONED;
              Exit;
             end
            else if WaitResult <> ERROR_SUCCESS then
             begin
              Result:=WaitResult;
              Exit;
             end;

            {Lock Synchronizer (No Wait)}
            Result:=SynchronizerReaderLockEx(Synchronizer,0);
            Exit;
           end;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Synchronizer}
      if Unlock then SpinUnlock(SynchronizerEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function SynchronizerReaderUnlock(Synchronizer:TSynchronizerHandle):LongWord;
{Unlock an existing Synchronizer entry

 If the Synchronizer is locked for reading then decrement the count

 If the count is greater than zero then return immediately

 If the count reaches zero then unlock the Synchronizer and release the first
 writer thread waiting for it to be unlocked

 If the Synchronizer is locked for writing then return an error

 If the Synchronizer is not locked then return an error}
{Synchronizer: Synchronizer to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Check the State}
 Result:=ERROR_NOT_LOCKED;
 if SynchronizerEntry.State <> SYNCHRONIZER_STATE_READER_LOCKED then Exit;

 {Check the Handler}
 if Assigned(SynchronizerReaderUnlockHandler) then
  begin
   {Use the Handler method}
   Result:=SynchronizerReaderUnlockHandler(SynchronizerEntry);
  end
 else
  begin
   {Use the Default method}
   {Note: Cannot shortcut ReaderUnlock on Synchronizer}
   {Lock the Synchronizer}
   if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      Result:=ERROR_INVALID_PARAMETER;
      if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

      {Check Reader Count}
      Result:=ERROR_INVALID_FUNCTION;
      if SynchronizerEntry.ReaderCount = 0 then Exit;

      {Update Reader Count}
      Dec(SynchronizerEntry.ReaderCount);

      {Update Reader Last}
      SynchronizerEntry.ReaderLast:=INVALID_HANDLE_VALUE;

      {Check Reader Count}
      if SynchronizerEntry.ReaderCount = 0 then
       begin
        {Set State}
        SynchronizerEntry.State:=SYNCHRONIZER_STATE_UNLOCKED;

        {Check Writer List}
        while ListNotEmpty(SynchronizerEntry.WriterList) do
         begin
          {Release one writer thread waiting on Synchronizer}
          if SynchronizerEntry.Release(SynchronizerEntry.WriterList) = ERROR_SUCCESS then
           begin
            Break;
           end;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Synchronizer}
      SpinUnlock(SynchronizerEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function SynchronizerReaderConvert(Synchronizer:TSynchronizerHandle):LongWord;
{Convert a reader lock on an existing Synchronizer entry to a writer lock

 If the Synchronizer is locked for reading then decrement the count

 If the count is greater than zero then wait to acquire the writer lock

 If the count reaches zero then convert to writer lock with the current
 thread as the owner

 If the Synchronizer is locked for writing then return an error

 If the Synchronizer is not locked then return an error}
{Synchronizer: Synchronizer to convert}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Since reader locks are recursive but do not maintain reader thread ownership,
 caller must ensure that one and only one reader lock is held by the current thread}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 {Check the Handler}
 if Assigned(SynchronizerReaderConvertHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check Synchronizer}
   if Synchronizer = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
   if SynchronizerEntry = nil then Exit;
   if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

   {Check the State}
   Result:=ERROR_NOT_LOCKED;
   if SynchronizerEntry.State <> SYNCHRONIZER_STATE_READER_LOCKED then Exit;

   {Use the Handler method}
   Result:=SynchronizerReaderConvertHandler(SynchronizerEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=SynchronizerReaderConvertEx(Synchronizer,INFINITE);
  end;
end;

{==============================================================================}

function SynchronizerReaderConvertEx(Synchronizer:TSynchronizerHandle;Timeout:LongWord):LongWord;
{Convert a reader lock on an existing Synchronizer entry to a writer lock

 If the Synchronizer is locked for reading then decrement the count

 If the count is greater than zero then wait to acquire the writer lock

 If the count reaches zero then convert to writer lock with the current
 thread as the owner

 If the Synchronizer is locked for writing then return an error

 If the Synchronizer is not locked then return an error}
{Synchronizer: Synchronizer to convert}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Since reader locks are recursive but do not maintain reader thread ownership,
 caller must ensure that one and only one reader lock is held by the current thread}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Check the State}
 Result:=ERROR_NOT_LOCKED;
 if SynchronizerEntry.State <> SYNCHRONIZER_STATE_READER_LOCKED then Exit;

 {Check the Handler}
 if Assigned(SynchronizerReaderConvertExHandler) then
  begin
   {Use the Handler method}
   Result:=SynchronizerReaderConvertExHandler(SynchronizerEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Synchronizer}
   if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      Result:=ERROR_INVALID_PARAMETER;
      if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check Reader State}
        Result:=ERROR_WAIT_TIMEOUT;
        if SynchronizerEntry.ReaderCount <> 1 then Exit;
       end;

      {Check Reader Count}
      Result:=ERROR_INVALID_FUNCTION;
      if SynchronizerEntry.ReaderCount = 0 then Exit;

      {Update Reader Count}
      Dec(SynchronizerEntry.ReaderCount);

      {Update Reader Last}
      SynchronizerEntry.ReaderLast:=INVALID_HANDLE_VALUE;

      {Check Reader Count}
      if SynchronizerEntry.ReaderCount = 0 then
       begin
        {Set State}
        SynchronizerEntry.State:=SYNCHRONIZER_STATE_WRITER_LOCKED;

        {Set Writer Count}
        SynchronizerEntry.WriterCount:=1;

        {Set Writer Owner}
        SynchronizerEntry.WriterOwner:=ThreadGetCurrent;
       end
      else
       begin
        {Check Writer List}
        if SynchronizerEntry.WriterList = INVALID_HANDLE_VALUE then
         begin
          {Create Writer List}
          SynchronizerEntry.WriterList:=ListCreateEx(LIST_TYPE_WAIT_SYNCHRONIZER,SchedulerGetListFlags(LIST_TYPE_WAIT_SYNCHRONIZER));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Synchronizer}
          SynchronizerEntry.Wait(SynchronizerEntry.WriterList,SynchronizerEntry.Lock,LOCK_FLAG_NONE);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;

          {Lock Synchronizer (Infinite Wait)}
          Result:=SynchronizerWriterLockEx(Synchronizer,INFINITE);
          Exit;
         end
        else
         begin
          {Wait on Synchronizer with Timeout}
          SynchronizerEntry.WaitEx(SynchronizerEntry.WriterList,SynchronizerEntry.Lock,LOCK_FLAG_NONE,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;

          {Lock Synchronizer (No Wait)}
          Result:=SynchronizerWriterLockEx(Synchronizer,0);
          Exit;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Synchronizer}
      if Unlock then SpinUnlock(SynchronizerEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function SynchronizerWriterCount(Synchronizer:TSynchronizerHandle):LongWord;
{Get the current writer count of an existing Synchronizer entry}
{Synchronizer: Synchronizer to get count for}
{Return: Current writer count or INVALID_HANDLE_VALUE on error}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Lock the Synchronizer}
 if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

    {Get Writer Count}
    Result:=SynchronizerEntry.WriterCount;
   finally
    {Unlock the Synchronizer}
    SpinUnlock(SynchronizerEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function SynchronizerWriterOwner(Synchronizer:TSynchronizerHandle):TThreadHandle;
{Get the current writer owner of an existing Synchronizer entry}
{Synchronizer: Synchronizer to get owner for}
{Return: Handle of owning thread or INVALID_HANDLE_VALUE if not currently owned}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Lock the Synchronizer}
 if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

    {Get Writer Owner}
    Result:=SynchronizerEntry.WriterOwner;
   finally
    {Unlock the Synchronizer}
    SpinUnlock(SynchronizerEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function SynchronizerWriterLock(Synchronizer:TSynchronizerHandle):LongWord;
{Lock an existing Synchronizer entry for writing

 If the Synchronizer is not locked then lock it, set the writer count to one
 and mark the owner as the current thread

 If the Synchronizer is already locked by the current thread then increment
 the writer count and return immediately

 If the Synchronizer is already locked for reading then wait until it is unlocked}
{Synchronizer: Synchronizer to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 {Check the Handler}
 if Assigned(SynchronizerWriterLockHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check Synchronizer}
   if Synchronizer = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
   if SynchronizerEntry = nil then Exit;
   if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

   {$IFDEF LOCK_DEBUG}
   if SCHEDULER_FIQ_ENABLED then
    begin
     if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(SynchronizerDeadlockCounter);
      end;
    end
   else
    begin
     if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(SynchronizerDeadlockCounter);
      end;
    end;
   if (SynchronizerEntry.ReaderLast = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SynchronizerRecursionCounter);
    end;
   {$ENDIF}

   {Use the Handler method}
   Result:=SynchronizerWriterLockHandler(SynchronizerEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=SynchronizerWriterLockEx(Synchronizer,INFINITE);
  end;
end;

{==============================================================================}

function SynchronizerWriterLockEx(Synchronizer:TSynchronizerHandle;Timeout:LongWord):LongWord;
{Lock an existing Synchronizer entry for writing

 If the Synchronizer is not locked then lock it, set the writer count to one
 and mark the owner as the current thread

 If the Synchronizer is already locked by the current thread then increment
 the writer count and return immediately

 If the Synchronizer is already locked for reading then wait until it is unlocked}
{Synchronizer: Synchronizer to lock}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SynchronizerDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(SynchronizerDeadlockCounter);
    end;
  end;
 if (SynchronizerEntry.ReaderLast = ThreadGetCurrent) and (InitializationCompleted[CPUGetCurrent]) then
  begin
   Inc(SynchronizerRecursionCounter);
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(SynchronizerWriterLockExHandler) then
  begin
   {Use the Handler method}
   Result:=SynchronizerWriterLockExHandler(SynchronizerEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Check State and Writer}
   if (SynchronizerEntry.State = SYNCHRONIZER_STATE_WRITER_LOCKED) and (SynchronizerEntry.WriterOwner = ThreadGetCurrent) then
    begin
     {Update Writer Count}
     Inc(SynchronizerEntry.WriterCount);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end
   else
    begin
     {Lock the Synchronizer}
     if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
      begin
       Unlock:=True;
       try
        {Check Signature}
        if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

        {Check Timeout}
        if Timeout = 0 then
         begin
          {Check Reader and Writer State}
          Result:=ERROR_WAIT_TIMEOUT;
          if SynchronizerEntry.ReaderCount <> 0 then Exit;
          if SynchronizerEntry.WriterCount <> 0 then Exit;
          if SynchronizerEntry.WriterOwner <> INVALID_HANDLE_VALUE then Exit;
         end;

        {Check State}
        if SynchronizerEntry.State = SYNCHRONIZER_STATE_UNLOCKED then
         begin
          {Set State}
          SynchronizerEntry.State:=SYNCHRONIZER_STATE_WRITER_LOCKED;

          {Set Writer Count}
          SynchronizerEntry.WriterCount:=1;

          {Set Writer Owner}
          SynchronizerEntry.WriterOwner:=ThreadGetCurrent;
         end
        else
         begin
          {Check Writer Owner}
          if SynchronizerEntry.WriterOwner = ThreadGetCurrent then
           begin
            {Update Writer Count}
            Inc(SynchronizerEntry.WriterCount);
           end
          else
           begin
            {Check Writer List}
            if SynchronizerEntry.WriterList = INVALID_HANDLE_VALUE then
             begin
              {Create Writer List}
              SynchronizerEntry.WriterList:=ListCreateEx(LIST_TYPE_WAIT_SYNCHRONIZER,SchedulerGetListFlags(LIST_TYPE_WAIT_SYNCHRONIZER));
             end;

            {Check Timeout}
            if Timeout = INFINITE then
             begin
              {Wait on Synchronizer}
              SynchronizerEntry.Wait(SynchronizerEntry.WriterList,SynchronizerEntry.Lock,LOCK_FLAG_NONE);
              Unlock:=False;

              {Check Result}
              WaitResult:=ThreadGetWaitResult;
              if WaitResult = WAIT_TIMEOUT then
               begin
                Result:=ERROR_WAIT_TIMEOUT;
                Exit;
               end
              else if WaitResult = WAIT_ABANDONED then
               begin
                Result:=ERROR_WAIT_ABANDONED;
                Exit;
               end
              else if WaitResult <> ERROR_SUCCESS then
               begin
                Result:=WaitResult;
                Exit;
               end;

              {Lock Synchronizer (Infinite Wait)}
              Result:=SynchronizerWriterLockEx(Synchronizer,INFINITE);
              Exit;
             end
            else
             begin
              {Wait on Synchronizer with Timeout}
              SynchronizerEntry.WaitEx(SynchronizerEntry.WriterList,SynchronizerEntry.Lock,LOCK_FLAG_NONE,Timeout);
              Unlock:=False;

              {Check Result}
              WaitResult:=ThreadGetWaitResult;
              if WaitResult = WAIT_TIMEOUT then
               begin
                Result:=ERROR_WAIT_TIMEOUT;
                Exit;
               end
              else if WaitResult = WAIT_ABANDONED then
               begin
                Result:=ERROR_WAIT_ABANDONED;
                Exit;
               end
              else if WaitResult <> ERROR_SUCCESS then
               begin
                Result:=WaitResult;
                Exit;
               end;

              {Lock Synchronizer (No Wait)}
              Result:=SynchronizerWriterLockEx(Synchronizer,0);
              Exit;
             end;
           end;
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       finally
        {Unlock the Synchronizer}
        if Unlock then SpinUnlock(SynchronizerEntry.Lock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
      end;
    end;
  end;
end;

{==============================================================================}

function SynchronizerWriterUnlock(Synchronizer:TSynchronizerHandle):LongWord;
{Unlock an existing Synchronizer entry

 If the Synchronizer is locked for writing by the current thread then decrement the count

 If the count is greater than zero then return immediately

 If the count reaches zero then unlock the Synchronizer and release all reader threads
 waiting for it to be unlocked or the first writer thread waiting for it to be unlocked

 If the Synchronizer is locked for reading then return an error

 If the Synchronizer is locked for writing by another thread then return an error

 If the Synchronizer is not locked then return an error}
{Synchronizer: Synchronizer to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Check the State}
 Result:=ERROR_NOT_LOCKED;
 if SynchronizerEntry.State <> SYNCHRONIZER_STATE_WRITER_LOCKED then Exit;

 {Check Writer Owner}
 Result:=ERROR_NOT_OWNER;
 if SynchronizerEntry.WriterOwner <> ThreadGetCurrent then Exit;

 {Check the Handler}
 if Assigned(SynchronizerWriterUnlockHandler) then
  begin
   {Use the Handler method}
   Result:=SynchronizerWriterUnlockHandler(SynchronizerEntry);
  end
 else
  begin
   {Use the Default method}
   {Check Writer Count}
   if SynchronizerEntry.WriterCount > 1 then
    begin
     {Update Writer Count}
     Dec(SynchronizerEntry.WriterCount);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end
   else
    begin
     {Lock the Synchronizer}
     if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Signature}
        Result:=ERROR_INVALID_PARAMETER;
        if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

        {Check Writer Count}
        Result:=ERROR_INVALID_FUNCTION;
        if SynchronizerEntry.WriterCount = 0 then Exit;

        {Update Writer Count}
        Dec(SynchronizerEntry.WriterCount);

        {Check Writer Count}
        if SynchronizerEntry.WriterCount = 0 then
         begin
          {Set State}
          SynchronizerEntry.State:=SYNCHRONIZER_STATE_UNLOCKED;

          {Set Writer Owner}
          SynchronizerEntry.WriterOwner:=INVALID_HANDLE_VALUE;

          {Check Reader List}
          if ListNotEmpty(SynchronizerEntry.ReaderList) then
           begin
            {Release all reader threads waiting on Synchronizer}
            while ListNotEmpty(SynchronizerEntry.ReaderList) do
             begin
              {Release reader thread waiting on Synchronizer}
              SynchronizerEntry.Release(SynchronizerEntry.ReaderList);
             end;
           end
          else
           begin
            {Check Writer List}
            while ListNotEmpty(SynchronizerEntry.WriterList) do
             begin
              {Release one writer thread waiting on Synchronizer}
              if SynchronizerEntry.Release(SynchronizerEntry.WriterList) = ERROR_SUCCESS then
               begin
                Break;
               end;
             end;
           end;
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       finally
        {Unlock the Synchronizer}
        SpinUnlock(SynchronizerEntry.Lock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
      end;
    end;
  end;
end;

{==============================================================================}

function SynchronizerWriterConvert(Synchronizer:TSynchronizerHandle):LongWord;
{Convert a writer lock on an existing Synchronizer entry to a reader lock

 If the Synchronizer is locked for writing by the current thread and the count
 is one then decrement the count

 If the count is greater than one then return an error

 If the count reaches zero then convert to reader lock and release all waiting
 reader threads

 If the Synchronizer is locked for reading then return an error

 If the Synchronizer is not locked then return an error}
{Synchronizer: Synchronizer to convert}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Since writer locks are recursive, caller must ensure that one and only
 one writer lock is held by the current thread}
var
 SynchronizerEntry:PSynchronizerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Synchronizer}
 if Synchronizer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 SynchronizerEntry:=PSynchronizerEntry(Synchronizer);
 if SynchronizerEntry = nil then Exit;
 if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

 {Check the State}
 Result:=ERROR_NOT_LOCKED;
 if SynchronizerEntry.State <> SYNCHRONIZER_STATE_WRITER_LOCKED then Exit;

 {Check Writer Owner}
 Result:=ERROR_NOT_OWNER;
 if SynchronizerEntry.WriterOwner <> ThreadGetCurrent then Exit;

 {Check the Handler}
 if Assigned(SynchronizerWriterConvertHandler) then
  begin
   {Use the Handler method}
   Result:=SynchronizerWriterConvertHandler(SynchronizerEntry);
  end
 else
  begin
   {Use the Default method}
   {Lock the Synchronizer}
   if SpinLock(SynchronizerEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      Result:=ERROR_INVALID_PARAMETER;
      if SynchronizerEntry.Signature <> SYNCHRONIZER_SIGNATURE then Exit;

      {Check Writer Count}
      Result:=ERROR_INVALID_FUNCTION;
      if SynchronizerEntry.WriterCount <> 1 then Exit;

      {Update Writer Count}
      Dec(SynchronizerEntry.WriterCount);

      {Check Writer Count}
      if SynchronizerEntry.WriterCount = 0 then
       begin
        {Set State}
        SynchronizerEntry.State:=SYNCHRONIZER_STATE_READER_LOCKED;

        {Set Reader Count}
        SynchronizerEntry.ReaderCount:=1;

        {Set Reader Last}
        SynchronizerEntry.ReaderLast:=ThreadGetCurrent;

        {Set Writer Owner}
        SynchronizerEntry.WriterOwner:=INVALID_HANDLE_VALUE;

        {Release all reader threads waiting on Synchronizer}
        while ListNotEmpty(SynchronizerEntry.ReaderList) do
         begin
          {Release reader thread waiting on Synchronizer}
          SynchronizerEntry.Release(SynchronizerEntry.ReaderList);
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       end;
     finally
      {Unlock the Synchronizer}
      SpinUnlock(SynchronizerEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Condition Functions}
function ConditionCreate:TConditionHandle;
{Create and insert a new Condition entry}
{Return: Handle of new Condition entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 ConditionEntry:PConditionEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create Condition entry}
 if CONDITION_SHARED_MEMORY then
  begin
   ConditionEntry:=AllocSharedMem(SizeOf(TConditionEntry));
  end
 else
  begin
   ConditionEntry:=AllocMem(SizeOf(TConditionEntry));
  end;
 if ConditionEntry = nil then Exit;

 {Setup Condition entry}
 ConditionEntry.Signature:=CONDITION_SIGNATURE;
 ConditionEntry.Flags:=CONDITION_FLAG_NONE;
 ConditionEntry.Lock:=SpinCreate;
 ConditionEntry.List:=INVALID_HANDLE_VALUE;
 ConditionEntry.Wait:=ThreadWait;
 ConditionEntry.WaitEx:=ThreadWaitEx;
 ConditionEntry.Release:=ThreadRelease;
 ConditionEntry.Abandon:=ThreadAbandon;

 {Insert Condition entry}
 if SpinLock(ConditionTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Condition entry}
    if ConditionTable = nil then
     begin
      ConditionTable:=ConditionEntry;
     end
    else
     begin
      ConditionEntry.Next:=ConditionTable;
      ConditionTable.Prev:=ConditionEntry;
      ConditionTable:=ConditionEntry;
     end;

    {Increment Condition Count}
    Inc(ConditionTableCount);

    {Return Condition entry}
    Result:=TConditionHandle(ConditionEntry);
   finally
    SpinUnlock(ConditionTableLock);
   end;
  end
 else
  begin
   {Free Condition Lock}
   SpinDestroy(ConditionEntry.Lock);

   {Free Condition Entry}
   FreeMem(ConditionEntry);
  end;
end;

{==============================================================================}

function ConditionDestroy(Condition:TConditionHandle):LongWord;
{Destroy and remove an existing Condition entry}
{Condition: Handle of Condition entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ConditionEntry:PConditionEntry;
 PrevEntry:PConditionEntry;
 NextEntry:PConditionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Condition}
 if Condition = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ConditionEntry:=PConditionEntry(Condition);
 if ConditionEntry = nil then Exit;
 if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

 {Remove Condition entry}
 if SpinLock(ConditionTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=SpinLock(ConditionEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if ConditionEntry.Signature <> CONDITION_SIGNATURE then
     begin
      {Release the Lock}
      Result:=SpinUnlock(ConditionEntry.Lock);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Condition entry}
    ConditionEntry.Signature:=0;

    {Check Waiting Threads}
    while ListNotEmpty(ConditionEntry.List) do
     begin
      {Abandon waiting thread}
      ConditionEntry.Abandon(ConditionEntry.List);
     end;

    {Unlink Condition entry}
    PrevEntry:=ConditionEntry.Prev;
    NextEntry:=ConditionEntry.Next;
    if PrevEntry = nil then
     begin
      ConditionTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Condition Count}
    Dec(ConditionTableCount);

    {Release the Lock}
    Result:=SpinUnlock(ConditionEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Free Condition List}
    if ConditionEntry.List <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(ConditionEntry.List);
     end;

    {Free Condition Lock}
    SpinDestroy(ConditionEntry.Lock);

    {Free Condition Entry}
    FreeMem(ConditionEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(ConditionTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConditionWait(Condition:TConditionHandle;Timeout:LongWord):LongWord;
{Wait on an existing Condition}
{Condition: Condition to wait on}
{Timeout: Time in milliseconds to wait to be woken
          0 = No Wait
          INFINITE = Wait Indefinitely}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 ConditionEntry:PConditionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Condition}
 if Condition = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ConditionEntry:=PConditionEntry(Condition);
 if ConditionEntry = nil then Exit;
 if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(ConditionWaitHandler) then
  begin
   {Use the Handler method}
   Result:=ConditionWaitHandler(ConditionEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Condition}
   if SpinLock(ConditionEntry.Lock) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Return Error}
        Result:=ERROR_WAIT_TIMEOUT;
       end
      else
       begin
        {Check List}
        if ConditionEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          ConditionEntry.List:=ListCreateEx(LIST_TYPE_WAIT_CONDITION,SchedulerGetListFlags(LIST_TYPE_WAIT_CONDITION));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Condition}
          ConditionEntry.Wait(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end
        else
         begin
          {Wait on Condition with Timeout}
          ConditionEntry.WaitEx(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       end;
     finally
      {Unlock the Condition}
      if Unlock then SpinUnlock(ConditionEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ConditionWaitMutex(Condition:TConditionHandle;Mutex:TMutexHandle;Timeout:LongWord):LongWord;
{Release a Mutex and Wait on an existing Condition in an atomic operation}
{Condition: Condition to wait on}
{Mutex: Mutex to release}
{Timeout: Time in milliseconds to wait to be woken
          0 = No Wait
          INFINITE = Wait Indefinitely}
{Return: ERROR_SUCCESS if completed or another error code on failure.
         Before returning (with either success or failure) the thread will reacquire the Mutex}

{Note: Caller must be the owner of the Mutex with a count of one on entry to this function}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 ConditionEntry:PConditionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Condition}
 if Condition = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ConditionEntry:=PConditionEntry(Condition);
 if ConditionEntry = nil then Exit;
 if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(ConditionWaitMutexHandler) then
  begin
   {Use the Handler method}
   Result:=ConditionWaitMutexHandler(ConditionEntry,Mutex,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Condition}
   if SpinLock(ConditionEntry.Lock) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

      Result:=ERROR_OPERATION_FAILED;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check Mutex}
        if MutexOwner(Mutex) <> ThreadGetCurrent then Exit;

        {Return Error}
        Result:=ERROR_WAIT_TIMEOUT;
       end
      else
       begin
        {Release the Mutex}
        if MutexUnlock(Mutex) <> ERROR_SUCCESS then Exit;

        {Check List}
        if ConditionEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          ConditionEntry.List:=ListCreateEx(LIST_TYPE_WAIT_CONDITION,SchedulerGetListFlags(LIST_TYPE_WAIT_CONDITION));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Condition}
          ConditionEntry.Wait(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end
        else
         begin
          {Wait on Condition with Timeout}
          ConditionEntry.WaitEx(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end;

        {Acquire the Mutex}
        if MutexLock(Mutex) <> ERROR_SUCCESS then Exit;

        {Return Result}
        Result:=ERROR_SUCCESS;
       end;
     finally
      {Unlock the Condition}
      if Unlock then SpinUnlock(ConditionEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ConditionWaitSynchronizer(Condition:TConditionHandle;Synchronizer:TSynchronizerHandle;Flags:LongWord;Timeout:LongWord):LongWord;
{Release a Synchronizer and Wait on an existing Condition in an atomic operation}
{Condition: Condition to wait on}
{Synchronizer: Synchronizer to release}
{Flags: Flags to indicate reader or writer lock for the Synchronizer (eg CONDITION_LOCK_FLAG_WRITER)}
{Timeout: Time in milliseconds to wait to be woken
          0 = No Wait
          INFINITE = Wait Indefinitely}
{Return: ERROR_SUCCESS if completed or another error code on failure.
         Before returning (with either success or failure) the thread will reacquire the Synchronizer
         for either reading or writing depending on the flags value}

{Note: Caller must be the owner of the Synchronizer with a count of one on entry to this function
       and the ownership must match the flags value provided}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 ConditionEntry:PConditionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Condition}
 if Condition = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ConditionEntry:=PConditionEntry(Condition);
 if ConditionEntry = nil then Exit;
 if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(ConditionWaitSynchronizerHandler) then
  begin
   {Use the Handler method}
   Result:=ConditionWaitSynchronizerHandler(ConditionEntry,Synchronizer,Flags,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Condition}
   if SpinLock(ConditionEntry.Lock) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

      Result:=ERROR_OPERATION_FAILED;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check Synchronizer}
        if (Flags and CONDITION_LOCK_FLAG_WRITER) <> 0 then
         begin
          if SynchronizerWriterOwner(Synchronizer) <> ThreadGetCurrent then Exit;
         end
        else
         begin
          if SynchronizerReaderCount(Synchronizer) = 0 then Exit;
         end;

        {Return Error}
        Result:=ERROR_WAIT_TIMEOUT;
       end
      else
       begin
        {Release the Synchronizer}
        if (Flags and CONDITION_LOCK_FLAG_WRITER) <> 0 then
         begin
          if SynchronizerWriterUnlock(Synchronizer) <> ERROR_SUCCESS then Exit;
         end
        else
         begin
          if SynchronizerReaderUnlock(Synchronizer) <> ERROR_SUCCESS then Exit;
         end;

        {Check List}
        if ConditionEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          ConditionEntry.List:=ListCreateEx(LIST_TYPE_WAIT_CONDITION,SchedulerGetListFlags(LIST_TYPE_WAIT_CONDITION));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Condition}
          ConditionEntry.Wait(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end
        else
         begin
          {Wait on Condition with Timeout}
          ConditionEntry.WaitEx(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end;

        {Acquire the Synchronizer}
        if (Flags and CONDITION_LOCK_FLAG_WRITER) <> 0 then
         begin
          if SynchronizerWriterLock(Synchronizer) <> ERROR_SUCCESS then Exit;
         end
        else
         begin
          if SynchronizerReaderLock(Synchronizer) <> ERROR_SUCCESS then Exit;
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       end;
     finally
      {Unlock the Condition}
      if Unlock then SpinUnlock(ConditionEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ConditionWaitCriticalSection(Condition:TConditionHandle;CriticalSection:TCriticalSectionHandle;Timeout:LongWord):LongWord;
{Release a Critical Section and Wait on an existing Condition in an atomic operation}
{Condition: Condition to wait on}
{CriticalSection: Critical Section to release}
{Timeout: Time in milliseconds to wait to be woken
          0 = No Wait
          INFINITE = Wait Indefinitely}
{Return: ERROR_SUCCESS if completed or another error code on failure.
         Before returning (with either success or failure) the thread will reacquire the Synchronizer
         for either reading or writing depending on the flags value}

{Note: Caller must be the owner of the Critical Section with a count of one on entry to this function}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 ConditionEntry:PConditionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Condition}
 if Condition = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ConditionEntry:=PConditionEntry(Condition);
 if ConditionEntry = nil then Exit;
 if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(ConditionWaitCriticalSectionHandler) then
  begin
   {Use the Handler method}
   Result:=ConditionWaitCriticalSectionHandler(ConditionEntry,CriticalSection,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Condition}
   if SpinLock(ConditionEntry.Lock) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

      Result:=ERROR_OPERATION_FAILED;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check Critical Section}
        if CriticalSectionOwner(CriticalSection) <> ThreadGetCurrent then Exit;

        {Return Error}
        Result:=ERROR_WAIT_TIMEOUT;
       end
      else
       begin
        {Release the Critical Section}
        if CriticalSectionUnlock(CriticalSection) <> ERROR_SUCCESS then Exit;

        {Check List}
        if ConditionEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          ConditionEntry.List:=ListCreateEx(LIST_TYPE_WAIT_CONDITION,SchedulerGetListFlags(LIST_TYPE_WAIT_CONDITION));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Condition}
          ConditionEntry.Wait(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end
        else
         begin
          {Wait on Condition with Timeout}
          ConditionEntry.WaitEx(ConditionEntry.List,ConditionEntry.Lock,LOCK_FLAG_NONE,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end;

        {Acquire the Critical Section}
        if CriticalSectionLock(CriticalSection) <> ERROR_SUCCESS then Exit;

        {Return Result}
        Result:=ERROR_SUCCESS;
       end;
     finally
      {Unlock the Condition}
      if Unlock then SpinUnlock(ConditionEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ConditionWake(Condition:TConditionHandle):LongWord;
{Wake one thread waiting on an existing Condition}
{Condition: Condition to wake}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ConditionEntry:PConditionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Condition}
 if Condition = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ConditionEntry:=PConditionEntry(Condition);
 if ConditionEntry = nil then Exit;
 if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(ConditionWakeHandler) then
  begin
   {Use the Handler method}
   Result:=ConditionWakeHandler(ConditionEntry);
  end
 else
  begin
   {Use the Default method}
   {Lock the Condition}
   if SpinLock(ConditionEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

      {Check List}
      while ListNotEmpty(ConditionEntry.List) do
       begin
        {Release one thread waiting on Condition}
        if ConditionEntry.Release(ConditionEntry.List) = ERROR_SUCCESS then
         begin
          Break;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Condition}
      SpinUnlock(ConditionEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ConditionWakeAll(Condition:TConditionHandle):LongWord;
{Wake all threads waiting on an existing Condition}
{Condition: Condition to wake}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ConditionEntry:PConditionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Condition}
 if Condition = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ConditionEntry:=PConditionEntry(Condition);
 if ConditionEntry = nil then Exit;
 if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(ConditionDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(ConditionWakeAllHandler) then
  begin
   {Use the Handler method}
   Result:=ConditionWakeAllHandler(ConditionEntry);
  end
 else
  begin
   {Use the Default method}
   {Lock the Condition}
   if SpinLock(ConditionEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if ConditionEntry.Signature <> CONDITION_SIGNATURE then Exit;

      {Check List}
      while ListNotEmpty(ConditionEntry.List) do
       begin
        {Release all threads waiting on Condition}
        ConditionEntry.Release(ConditionEntry.List);
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Condition}
      SpinUnlock(ConditionEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Completion Functions}
function CompletionLock(Completion:PCompletionEntry):LongWord; {$IFDEF COMPLETION_INLINE}inline;{$ENDIF}
{Internal function to lock an existing Completion entry based on the entry flags}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Completion}
 if Completion = nil then Exit;

 {Acquire the Lock}
 if (Completion.Flags and COMPLETION_FLAG_IRQFIQ) <> 0 then
  begin
   Result:=SpinLockIRQFIQ(Completion.Lock);
  end
 else if (Completion.Flags and COMPLETION_FLAG_FIQ) <> 0 then
  begin
   Result:=SpinLockFIQ(Completion.Lock);
  end
 else if (Completion.Flags and COMPLETION_FLAG_IRQ) <> 0 then
  begin
   Result:=SpinLockIRQ(Completion.Lock);
  end
 else
  begin
   Result:=SpinLock(Completion.Lock);
  end;
end;

{==============================================================================}

function CompletionUnlock(Completion:PCompletionEntry):LongWord; {$IFDEF COMPLETION_INLINE}inline;{$ENDIF}
{Internal function to unlock an existing Completion entry based on the entry flags}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Completion}
 if Completion = nil then Exit;

 {Release the Lock}
 if (Completion.Flags and COMPLETION_FLAG_IRQFIQ) <> 0 then
  begin
   Result:=SpinUnlockIRQFIQ(Completion.Lock);
  end
 else if (Completion.Flags and COMPLETION_FLAG_FIQ) <> 0 then
  begin
   Result:=SpinUnlockFIQ(Completion.Lock);
  end
 else if (Completion.Flags and COMPLETION_FLAG_IRQ) <> 0 then
  begin
   Result:=SpinUnlockIRQ(Completion.Lock);
  end
 else
  begin
   Result:=SpinUnlock(Completion.Lock);
  end;
end;

{==============================================================================}

function CompletionCreate(Flags:LongWord):TCompletionHandle;
{Create and insert a new Completion entry}
{Flags: The flags for the Completion entry (eg COMPLETION_FLAG_IRQ)}
{Return: Handle of new Completion entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 CompletionEntry:PCompletionEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create Completion entry}
 if COMPLETION_SHARED_MEMORY then
  begin
   CompletionEntry:=AllocSharedMem(SizeOf(TCompletionEntry));
  end
 else
  begin
   CompletionEntry:=AllocMem(SizeOf(TCompletionEntry));
  end;
 if CompletionEntry = nil then Exit;

 {Setup Completion entry}
 CompletionEntry.Signature:=COMPLETION_SIGNATURE;
 CompletionEntry.State:=COMPLETION_STATE_RESET;
 CompletionEntry.Count:=0;
 CompletionEntry.Flags:=Flags;
 CompletionEntry.Lock:=SpinCreate;
 CompletionEntry.List:=INVALID_HANDLE_VALUE;
 CompletionEntry.Wait:=ThreadWait;
 CompletionEntry.WaitEx:=ThreadWaitEx;
 CompletionEntry.Release:=ThreadRelease;
 CompletionEntry.Abandon:=ThreadAbandon;

 {Check Completion flags}
 if (Flags and (COMPLETION_FLAG_IRQ or COMPLETION_FLAG_FIQ or COMPLETION_FLAG_IRQFIQ)) <> 0 then
  begin
   {Create Completion List}
   CompletionEntry.List:=ListCreateEx(LIST_TYPE_WAIT_COMPLETION,SchedulerGetListFlags(LIST_TYPE_WAIT_COMPLETION));
  end;

 {Insert Completion entry}
 if SpinLock(CompletionTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Completion entry}
    if CompletionTable = nil then
     begin
      CompletionTable:=CompletionEntry;
     end
    else
     begin
      CompletionEntry.Next:=CompletionTable;
      CompletionTable.Prev:=CompletionEntry;
      CompletionTable:=CompletionEntry;
     end;

    {Increment Completion Count}
    Inc(CompletionTableCount);

    {Return Completion entry}
    Result:=TCompletionHandle(CompletionEntry);
   finally
    SpinUnlock(CompletionTableLock);
   end;
  end
 else
  begin
   {Free Completion List}
   if CompletionEntry.List <> INVALID_HANDLE_VALUE then
    begin
     ListDestroy(CompletionEntry.List);
    end;

   {Free Completion Lock}
   SpinDestroy(CompletionEntry.Lock);

   {Free Completion Entry}
   FreeMem(CompletionEntry);
  end;
end;

{==============================================================================}

function CompletionDestroy(Completion:TCompletionHandle):LongWord;
{Destroy and remove an existing Completion entry}
{Completion: Handle of Completion entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 CompletionEntry:PCompletionEntry;
 PrevEntry:PCompletionEntry;
 NextEntry:PCompletionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CompletionEntry:=PCompletionEntry(Completion);
 if CompletionEntry = nil then Exit;
 if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

 {Remove Completion entry}
 if SpinLock(CompletionTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=CompletionLock(CompletionEntry);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if CompletionEntry.Signature <> COMPLETION_SIGNATURE then
     begin
      {Release the Lock}
      Result:=CompletionUnlock(CompletionEntry);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Completion entry}
    CompletionEntry.Signature:=0;

    {Check Waiting Threads}
    while ListNotEmpty(CompletionEntry.List) do
     begin
      {Abandon waiting thread}
      CompletionEntry.Abandon(CompletionEntry.List);
     end;

    {Unlink Completion entry}
    PrevEntry:=CompletionEntry.Prev;
    NextEntry:=CompletionEntry.Next;
    if PrevEntry = nil then
     begin
      CompletionTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Completion Count}
    Dec(CompletionTableCount);

    {Release the Lock}
    Result:=CompletionUnlock(CompletionEntry);
    if Result <> ERROR_SUCCESS then Exit;

    {Free Completion List}
    if CompletionEntry.List <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(CompletionEntry.List);
     end;

    {Free Completion Lock}
    SpinDestroy(CompletionEntry.Lock);

    {Free Completion Entry}
    FreeMem(CompletionEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(CompletionTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CompletionState(Completion:TCompletionHandle):LongWord;
{Get the current state of an existing Completion entry}
{Completion: Completion to get the state for}
{Return: Current state (eg COMPLETION_STATE_COMPLETE) or INVALID_HANDLE_VALUE on error}
var
 CompletionEntry:PCompletionEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CompletionEntry:=PCompletionEntry(Completion);
 if CompletionEntry = nil then Exit;
 if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

 {Lock the Completion}
 if CompletionLock(CompletionEntry) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

    {Get the State}
    Result:=CompletionEntry.State;
   finally
    {Unlock the Completion}
    CompletionUnlock(CompletionEntry);
   end;
  end;
end;

{==============================================================================}

function CompletionWait(Completion:TCompletionHandle;Timeout:LongWord):LongWord;
{Wait on an existing Completion

 If the completion is set (complete) then return immediately with success

 If the completion is not set then wait for it to be completed before
 returning

 For counted completions, decrement the count if it is not 0 or -1 after
 testing if the completion is set}
{Completion: Completion to wait on}
{Timeout: Time in milliseconds to wait to be woken
          0 = No Wait
          INFINITE = Wait Indefinitely}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 CompletionEntry:PCompletionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CompletionEntry:=PCompletionEntry(Completion);
 if CompletionEntry = nil then Exit;
 if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(CompletionWaitHandler) then
  begin
   {Use the Handler method}
   Result:=CompletionWaitHandler(CompletionEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {$IFDEF LOCK_DEBUG}
   if (CompletionEntry.Flags and (COMPLETION_FLAG_IRQ or COMPLETION_FLAG_FIQ or COMPLETION_FLAG_IRQFIQ)) = 0 then
    begin
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end;
    end;
   {$ENDIF}

   {Lock the Completion}
   if CompletionLock(CompletionEntry) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check State}
        Result:=ERROR_WAIT_TIMEOUT;
        if CompletionEntry.State = COMPLETION_STATE_RESET then Exit;
       end;

      {Check State}
      if CompletionEntry.State = COMPLETION_STATE_COMPLETE then
       begin
        {Check Counted}
        if (CompletionEntry.Flags and COMPLETION_FLAG_COUNTED) <> 0 then
         begin
          {Check Count}
          if CompletionEntry.Count = 0 then
           begin
            Result:=ERROR_INVALID_FUNCTION;
            Exit;
           end;

          {Check Count}
          if CompletionEntry.Count <> LongWord(-1) then
           begin
            {Decrement Count}
            Dec(CompletionEntry.Count);
           end;

          {Check Count}
          if CompletionEntry.Count = 0 then
           begin
            {Reset State}
            CompletionEntry.State:=COMPLETION_STATE_RESET;
           end;
         end;
       end
      else
       begin
        {Check List}
        if CompletionEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          CompletionEntry.List:=ListCreateEx(LIST_TYPE_WAIT_COMPLETION,SchedulerGetListFlags(LIST_TYPE_WAIT_COMPLETION));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Completion}
          CompletionEntry.Wait(CompletionEntry.List,CompletionEntry.Lock,CompletionEntry.Flags);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end
        else
         begin
          {Wait on Completion with Timeout}
          CompletionEntry.WaitEx(CompletionEntry.List,CompletionEntry.Lock,CompletionEntry.Flags,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Completion}
      if Unlock then CompletionUnlock(CompletionEntry);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function CompletionTryWait(Completion:TCompletionHandle):LongWord;
{Try an existing Completion to see if it is completed

 If the completion is not set (complete) then return immediately with an error
 and do not wait for it to be completed}
{Completion: Completion to try}
{Return: ERROR_SUCCESS if completed, ERROR_NOT_READY if not completed or another error code on failure}
begin
 {}
 Result:=CompletionWait(Completion,0);
 if Result = ERROR_WAIT_TIMEOUT then Result:=ERROR_NOT_READY;
end;

{==============================================================================}

function CompletionReset(Completion:TCompletionHandle):LongWord;
{Reset (uncomplete) the state of an existing Completion entry

 If the completion is not set then return with no action

 If the completion is set then change the state to not set

 For counted completions, reset the counter to 0}
{Completion: Completion to reset the state for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 CompletionEntry:PCompletionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CompletionEntry:=PCompletionEntry(Completion);
 if CompletionEntry = nil then Exit;
 if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(CompletionResetHandler) then
  begin
   {Use the Handler method}
   Result:=CompletionResetHandler(CompletionEntry);
  end
 else
  begin
   {Use the Default method}
   {$IFDEF LOCK_DEBUG}
   if (CompletionEntry.Flags and (COMPLETION_FLAG_IRQ or COMPLETION_FLAG_FIQ or COMPLETION_FLAG_IRQFIQ)) = 0 then
    begin
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end;
    end;
   {$ENDIF}

   {Lock the Completion}
   if CompletionLock(CompletionEntry) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

      {Check State}
      if CompletionEntry.State = COMPLETION_STATE_COMPLETE then
       begin
        {Reset State}
        CompletionEntry.State:=COMPLETION_STATE_RESET;

        {Reset Count}
        CompletionEntry.Count:=0;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Completion}
      CompletionUnlock(CompletionEntry);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function CompletionComplete(Completion:TCompletionHandle):LongWord;
{Set (complete) the state of an existing Completion entry

 If the completion is already set then return with no action

 If the completion is not set then release one waiting thread (if any)
 and return

 For counted completions, release one waiting thread, if there are no
 waiting threads increment the count if it is not -1 and return}
{Completion: Completion to set the state for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 CompletionEntry:PCompletionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CompletionEntry:=PCompletionEntry(Completion);
 if CompletionEntry = nil then Exit;
 if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(CompletionCompleteHandler) then
  begin
   {Use the Handler method}
   Result:=CompletionCompleteHandler(CompletionEntry);
  end
 else
  begin
   {Use the Default method}
   {$IFDEF LOCK_DEBUG}
   if (CompletionEntry.Flags and (COMPLETION_FLAG_IRQ or COMPLETION_FLAG_FIQ or COMPLETION_FLAG_IRQFIQ)) = 0 then
    begin
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end;
    end;
   {$ENDIF}

   {Lock the Completion}
   if CompletionLock(CompletionEntry) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

      {Check Counted}
      if (CompletionEntry.Flags and COMPLETION_FLAG_COUNTED) <> 0 then
       begin
        {Check List}
        if ListIsEmpty(CompletionEntry.List) then
         begin
          {Check Count}
          if CompletionEntry.Count <> LongWord(-1) then
           begin
            {Set State}
            CompletionEntry.State:=COMPLETION_STATE_COMPLETE;

            {Increment Count}
            Inc(CompletionEntry.Count);
           end;
         end
        else
         begin
          {Check List}
          while ListNotEmpty(CompletionEntry.List) do
           begin
            {Release one thread waiting on Completion}
            if CompletionEntry.Release(CompletionEntry.List) = ERROR_SUCCESS then
             begin
              Break;
             end;
           end;
         end;
       end
      else
       begin
        {Check State}
        if CompletionEntry.State = COMPLETION_STATE_RESET then
         begin
          {Check List}
          while ListNotEmpty(CompletionEntry.List) do
           begin
            {Release one thread waiting on Completion}
            if CompletionEntry.Release(CompletionEntry.List) = ERROR_SUCCESS then
             begin
              Break;
             end;
           end;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Completion}
      CompletionUnlock(CompletionEntry);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function CompletionCompleteAll(Completion:TCompletionHandle):LongWord;
{Set (complete) the state of an existing Completion entry

 If the completion is already set then return with no action

 If the completion is not set then release all waiting threads (if any)
 and return

 For counted completions, set the count to -1, release all waiting threads
 (if any) and return}
{Completion: Completion to set the state for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 CompletionEntry:PCompletionEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 CompletionEntry:=PCompletionEntry(Completion);
 if CompletionEntry = nil then Exit;
 if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(CompletionCompleteAllHandler) then
  begin
   {Use the Handler method}
   Result:=CompletionCompleteAllHandler(CompletionEntry);
  end
 else
  begin
   {Use the Default method}
   {$IFDEF LOCK_DEBUG}
   if (CompletionEntry.Flags and (COMPLETION_FLAG_IRQ or COMPLETION_FLAG_FIQ or COMPLETION_FLAG_IRQFIQ)) = 0 then
    begin
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(CompletionDeadlockCounter);
        end;
      end;
    end;
   {$ENDIF}

   {Lock the Completion}
   if CompletionLock(CompletionEntry) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if CompletionEntry.Signature <> COMPLETION_SIGNATURE then Exit;

      {Check State}
      if CompletionEntry.State = COMPLETION_STATE_RESET then
       begin
        {Set State}
        CompletionEntry.State:=COMPLETION_STATE_COMPLETE;

        {Set Count}
        CompletionEntry.Count:=LongWord(-1);

        {Check List}
        while ListNotEmpty(CompletionEntry.List) do
         begin
          {Release all threads waiting on Completion}
          CompletionEntry.Release(CompletionEntry.List);
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Completion}
      CompletionUnlock(CompletionEntry);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{List Functions}
function ListCreate:TListHandle; {$IFDEF LIST_INLINE}inline;{$ENDIF}
{Create and insert a new List entry}
{Return: Handle of new List entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=ListCreateEx(LIST_TYPE_NOT_SPECIFIED,LIST_FLAG_NONE);
end;

{==============================================================================}

function ListCreateEx(ListType:LongWord;Flags:LongWord):TListHandle;
{Create and insert a new List entry}
{ListType: Type of list to create (eg LIST_TYPE_WAIT_SEMAPHORE)}
{Flags: Flags for the new list (eg LIST_FLAG_IRQ)}
{Return: Handle of new List entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create List entry}
 ListEntry:=AllocMem(SizeOf(TListEntry));
 if ListEntry = nil then Exit;

 {Setup List entry}
 ListEntry.Signature:=LIST_SIGNATURE;
 ListEntry.ListType:=ListType;
 ListEntry.Count:=0;
 ListEntry.Flags:=Flags;
 ListEntry.Lock:=SpinCreate;

 {Insert List entry}
 if SpinLock(ListTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link List entry}
    if ListTable = nil then
     begin
      ListTable:=ListEntry;
     end
    else
     begin
      ListEntry.Next:=ListTable;
      ListTable.Prev:=ListEntry;
      ListTable:=ListEntry;
     end;

    {Increment List Count}
    Inc(ListTableCount);

    {Return List entry}
    Result:=TListHandle(ListEntry);
   finally
    SpinUnlock(ListTableLock);
   end;
  end
 else
  begin
   {Free List Lock}
   SpinDestroy(ListEntry.Lock);

   {Free List Entry}
   FreeMem(ListEntry);
  end;
end;

{==============================================================================}

function ListDestroy(List:TListHandle):LongWord;
{Destroy and remove an existing List entry}
{List: Handle of List entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
 PrevEntry:PListEntry;
 NextEntry:PListEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Remove List entry}
 if SpinLock(ListTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock (Done locally due to invalidate below)}
    if (ListEntry.Flags and LIST_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinLockIRQFIQ(ListEntry.Lock);
     end
    else if (ListEntry.Flags and LIST_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinLockFIQ(ListEntry.Lock);
     end
    else if (ListEntry.Flags and LIST_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinLockIRQ(ListEntry.Lock);
     end
    else
     begin
      Result:=SpinLock(ListEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then
     begin
      {Release the Lock (Done locally due to invalidate below)}
      if (ListEntry.Flags and LIST_FLAG_IRQFIQ) <> 0 then
       begin
        Result:=SpinUnlockIRQFIQ(ListEntry.Lock);
       end
      else if (ListEntry.Flags and LIST_FLAG_FIQ) <> 0 then
       begin
        Result:=SpinUnlockFIQ(ListEntry.Lock);
       end
      else if (ListEntry.Flags and LIST_FLAG_IRQ) <> 0 then
       begin
        Result:=SpinUnlockIRQ(ListEntry.Lock);
       end
      else
       begin
        Result:=SpinUnlock(ListEntry.Lock);
       end;
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate List entry}
    ListEntry.Signature:=0;

    {Unlink List entry}
    PrevEntry:=ListEntry.Prev;
    NextEntry:=ListEntry.Next;
    if PrevEntry = nil then
     begin
      ListTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement List Count}
    Dec(ListTableCount);

    {Release the Lock (Done locally due to invalidate above)}
    if (ListEntry.Flags and LIST_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinUnlockIRQFIQ(ListEntry.Lock);
     end
    else if (ListEntry.Flags and LIST_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinUnlockFIQ(ListEntry.Lock);
     end
    else if (ListEntry.Flags and LIST_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinUnlockIRQ(ListEntry.Lock);
     end
    else
     begin
      Result:=SpinUnlock(ListEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Free List Lock}
    SpinDestroy(ListEntry.Lock);

    {Free List Entry}
    FreeMem(ListEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(ListTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ListCount(List:TListHandle):LongWord;
{Get the current count from the supplied list}
{List: Handle of List entry to get from}
{Return: List count on success or INVALID_HANDLE_VALUE on failure}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Get Count}
 Result:=ListEntry.Count;
end;

{==============================================================================}

function ListAddFirst(List:TListHandle;Element:PListElement):LongWord;
{Add the supplied element as the first item in the List}
{List: Handle of List entry to add to}
{Element: The list element to be added}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Element}
 if Element = nil then Exit;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Check First}
    if ListEntry.First = nil then
     begin
      Element.Next:=nil;
      Element.Prev:=nil;
      ListEntry.First:=Element;
      ListEntry.Last:=Element;
     end
    else
     begin
      Element.Next:=ListEntry.First;
      Element.Prev:=nil;
      ListEntry.First.Prev:=Element;
      ListEntry.First:=Element;
     end;

    {Increment Count}
    Inc(ListEntry.Count);

    Result:=ERROR_SUCCESS;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ListAddLast(List:TListHandle;Element:PListElement):LongWord;
{Add the supplied element as the last item in the List}
{List: Handle of List entry to add to}
{Element: The list element to be added}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Element}
 if Element = nil then Exit;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Check Last}
    if ListEntry.Last = nil then
     begin
      Element.Next:=nil;
      Element.Prev:=nil;
      ListEntry.First:=Element;
      ListEntry.Last:=Element;
     end
    else
     begin
      Element.Next:=nil;
      Element.Prev:=ListEntry.Last;
      ListEntry.Last.Next:=Element;
      ListEntry.Last:=Element;
     end;

    {Increment Count}
    Inc(ListEntry.Count);

    Result:=ERROR_SUCCESS;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ListGetThread(List:TListHandle;Thread:TThreadHandle):PListElement;
{Find the supplied thread in the List and return its element}
{List: Handle of List entry to find from}
{Thread: The thread handle to be found}
{Return: List element on success, nil on failure or list empty}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
 ListElement:PListElement;
begin
 {}
 Result:=nil;

 {Check the Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Get First}
    ListElement:=ListEntry.First;
    while ListElement <> nil do
     begin
      if ListElement.Thread = Thread then
       begin
        Result:=ListElement;
        Exit;
       end;
      ListElement:=ListElement.Next;
     end;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ListGetFirst(List:TListHandle):PListElement; {$IFDEF LIST_INLINE}inline;{$ENDIF}
{Get the first element from the List}
{List: Handle of List entry to get from}
{Return: List element on success, nil on failure or list empty}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
begin
 {}
 Result:=ListGetFirstEx(List,False);
end;

{==============================================================================}

function ListGetFirstEx(List:TListHandle;Remove:Boolean):PListElement;
{Get the first element from the List}
{List: Handle of List entry to get from}
{Remove: If true then remove the element from the list}
{Return: List element on success, nil on failure or list empty}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
 NextElement:PListElement;
begin
 {}
 Result:=nil;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Get First Element}
    Result:=ListEntry.First;

    {Check Remove}
    if (Result <> nil) and Remove then
     begin
      {Get Next}
      NextElement:=Result.Next;

      ListEntry.First:=NextElement;
      {Check Next}
      if NextElement = nil then
       begin
        ListEntry.Last:=nil;
       end
      else
       begin
        NextElement.Prev:=nil;
       end;

      {Decrement Count}
      Dec(ListEntry.Count);
     end;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ListGetLast(List:TListHandle):PListElement; {$IFDEF LIST_INLINE}inline;{$ENDIF}
{Get the last element from the List}
{List: Handle of List entry to get from}
{Return: List element on success, nil on failure or list empty}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
begin
 {}
 Result:=ListGetLastEx(List,False);
end;

{==============================================================================}

function ListGetLastEx(List:TListHandle;Remove:Boolean):PListElement;
{Get the last element from the List}
{List: Handle of List entry to get from}
{Remove: If true then remove the element from the list}
{Return: List element on success, nil on failure or list empty}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
 PrevElement:PListElement;
begin
 {}
 Result:=nil;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Get Last Element}
    Result:=ListEntry.Last;

    {Check Remove}
    if (Result <> nil) and Remove then
     begin
      {Get Prev}
      PrevElement:=Result.Prev;

      ListEntry.Last:=PrevElement;
      {Check Prev}
      if PrevElement = nil then
       begin
        ListEntry.First:=nil;
       end
      else
       begin
        PrevElement.Next:=nil;
       end;

      {Decrement Count}
      Dec(ListEntry.Count);
     end;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ListInsert(List:TListHandle;Previous,Element:PListElement):LongWord;
{Insert a new element in the List}
{List: Handle of List entry to insert into}
{Previous: The element to insert the new element after}
{Element: The list element to be inserted}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
 PrevElement:PListElement;
 NextElement:PListElement;
begin
 {}
 {Check Previous}
 if Previous = nil then
  begin
   {Add First}
   Result:=ListAddFirst(List,Element);
  end
 else
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check the Element}
   if Element = nil then Exit;

   {Check List}
   if List = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   ListEntry:=PListEntry(List);
   if ListEntry = nil then Exit;
   if ListEntry.Signature <> LIST_SIGNATURE then Exit;

   {Lock the List}
   if ListLock(List) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if ListEntry.Signature <> LIST_SIGNATURE then Exit;

      {Get Prev/Next}
      PrevElement:=Previous;
      NextElement:=Previous.Next;
      Element.Prev:=PrevElement;
      Element.Next:=NextElement;
      {Check Prev}
      if PrevElement = nil then
       begin
        ListEntry.First:=Element;
        {Check Next}
        if NextElement = nil then
         begin
          ListEntry.Last:=Element;
         end
        else
         begin
          NextElement.Prev:=Element;
         end;
       end
      else
       begin
        PrevElement.Next:=Element;
        {Check Next}
        if NextElement = nil then
         begin
          ListEntry.Last:=Element;
         end
        else
         begin
          NextElement.Prev:=Element;
         end;
       end;

      {Increment Count}
      Inc(ListEntry.Count);

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the List}
      ListUnlock(List);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ListRemove(List:TListHandle;Element:PListElement):LongWord;
{Remove an element from the List}
{List: Handle of List entry to remove from}
{Element: The list element to be removed}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
 PrevElement:PListElement;
 NextElement:PListElement;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Element}
 if Element = nil then Exit;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Get Prev/Next}
    PrevElement:=Element.Prev;
    NextElement:=Element.Next;
    {Check Prev}
    if PrevElement = nil then
     begin
      ListEntry.First:=NextElement;
      {Check Next}
      if NextElement = nil then
       begin
        ListEntry.Last:=nil;
       end
      else
       begin
        NextElement.Prev:=nil;
       end;
     end
    else
     begin
      PrevElement.Next:=NextElement;
      {Check Next}
      if NextElement = nil then
       begin
        ListEntry.Last:=PrevElement;
       end
      else
       begin
        NextElement.Prev:=PrevElement;
       end;
     end;

    {Decrement Count}
    Dec(ListEntry.Count);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ListIsEmpty(List:TListHandle):Boolean;
{Check if the supplied List is empty}
{List: Handle of List entry to check}
{Return: True if List is empty or does not exist, False if List is not empty}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=True; {Default to True}

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Check Empty}
    if ListEntry.First = nil then Exit;

    Result:=False;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ListNotEmpty(List:TListHandle):Boolean;
{Check if the supplied List is empty}
{List: Handle of List entry to check}
{Return: True if List is not empty, False if List is empty or does not exist}
{Note: If list is part of a synchronisation object then caller must hold the lock on the object containing the list}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=False;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Lock the List}
 if ListLock(List) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ListEntry.Signature <> LIST_SIGNATURE then Exit;

    {Check Empty}
    if ListEntry.First = nil then Exit;

    Result:=True;
   finally
    {Unlock the List}
    ListUnlock(List);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ListLock(List:TListHandle):LongWord; {$IFDEF LIST_INLINE}inline;{$ENDIF}
{Lock the supplied List}
{List: Handle of List entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Acquire the Lock}
 if (ListEntry.Flags and LIST_FLAG_IRQFIQ) <> 0 then
  begin
   Result:=SpinLockIRQFIQ(ListEntry.Lock);
  end
 else if (ListEntry.Flags and LIST_FLAG_FIQ) <> 0 then
  begin
   Result:=SpinLockFIQ(ListEntry.Lock);
  end
 else if (ListEntry.Flags and LIST_FLAG_IRQ) <> 0 then
  begin
   Result:=SpinLockIRQ(ListEntry.Lock);
  end
 else
  begin
   Result:=SpinLock(ListEntry.Lock);
  end;
end;

{==============================================================================}

function ListUnlock(List:TListHandle):LongWord; {$IFDEF LIST_INLINE}inline;{$ENDIF}
{Unlock the supplied List}
{List: Handle of List entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ListEntry:PListEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ListEntry:=PListEntry(List);
 if ListEntry = nil then Exit;
 if ListEntry.Signature <> LIST_SIGNATURE then Exit;

 {Release the Lock}
 if (ListEntry.Flags and LIST_FLAG_IRQFIQ) <> 0 then
  begin
   Result:=SpinUnlockIRQFIQ(ListEntry.Lock);
  end
 else if (ListEntry.Flags and LIST_FLAG_FIQ) <> 0 then
  begin
   Result:=SpinUnlockFIQ(ListEntry.Lock);
  end
 else if (ListEntry.Flags and LIST_FLAG_IRQ) <> 0 then
  begin
   Result:=SpinUnlockIRQ(ListEntry.Lock);
  end
 else
  begin
   Result:=SpinUnlock(ListEntry.Lock);
  end;
end;

{==============================================================================}
{==============================================================================}
{Queue Functions}
function QueueCreate:TQueueHandle; {$IFDEF QUEUE_INLINE}inline;{$ENDIF}
{Create and insert a new Queue entry}
{Return: Handle of new Queue entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=QueueCreateEx(QUEUE_TYPE_NOT_SPECIFIED,QUEUE_FLAG_NONE);
end;

{==============================================================================}

function QueueCreateEx(QueueType:LongWord;Flags:LongWord):TQueueHandle;
{Create and insert a new Queue entry}
{QueueType: Type of queue to create (eg QUEUE_TYPE_SCHEDULE_SLEEP)}
{Flags: Flags for the new queue (eg QUEUE_FLAG_DESCENDING)}
{Return: Handle of new Queue entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 QueueEntry:PQueueEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create Queue entry}
 QueueEntry:=AllocMem(SizeOf(TQueueEntry));
 if QueueEntry = nil then Exit;

 {Setup Queue entry}
 QueueEntry.Signature:=QUEUE_SIGNATURE;
 QueueEntry.QueueType:=QueueType;
 QueueEntry.Count:=0;
 QueueEntry.Flags:=Flags;
 QueueEntry.Lock:=SpinCreate;

 {Insert Queue entry}
 if SpinLock(QueueTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Queue entry}
    if QueueTable = nil then
     begin
      QueueTable:=QueueEntry;
     end
    else
     begin
      QueueEntry.Next:=QueueTable;
      QueueTable.Prev:=QueueEntry;
      QueueTable:=QueueEntry;
     end;

    {Increment Queue Count}
    Inc(QueueTableCount);

    {Return Queue entry}
    Result:=TQueueHandle(QueueEntry);
   finally
    SpinUnlock(QueueTableLock);
   end;
  end
 else
  begin
   {Free Queue Lock}
   SpinDestroy(QueueEntry.Lock);

   {Free Queue Entry}
   FreeMem(QueueEntry);
  end;
end;

{==============================================================================}

function QueueDestroy(Queue:TQueueHandle):LongWord;
{Destroy and remove an existing Queue entry}
{Queue: Handle of Queue entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 QueueEntry:PQueueEntry;
 PrevEntry:PQueueEntry;
 NextEntry:PQueueEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Remove Queue entry}
 if SpinLock(QueueTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock (Done locally due to invalidate below)}
    if (QueueEntry.Flags and QUEUE_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinLockIRQFIQ(QueueEntry.Lock);
     end
    else if (QueueEntry.Flags and QUEUE_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinLockFIQ(QueueEntry.Lock);
     end
    else if (QueueEntry.Flags and QUEUE_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinLockIRQ(QueueEntry.Lock);
     end
    else
     begin
      Result:=SpinLock(QueueEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then
     begin
      {Release the Lock (Done locally due to invalidate below)}
      if (QueueEntry.Flags and QUEUE_FLAG_IRQFIQ) <> 0 then
       begin
        Result:=SpinUnlockIRQFIQ(QueueEntry.Lock);
       end
      else if (QueueEntry.Flags and QUEUE_FLAG_FIQ) <> 0 then
       begin
        Result:=SpinUnlockFIQ(QueueEntry.Lock);
       end
      else if (QueueEntry.Flags and QUEUE_FLAG_IRQ) <> 0 then
       begin
        Result:=SpinUnlockIRQ(QueueEntry.Lock);
       end
      else
       begin
        Result:=SpinUnlock(QueueEntry.Lock);
       end;
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Queue entry}
    QueueEntry.Signature:=0;

    {Unlink Queue entry}
    PrevEntry:=QueueEntry.Prev;
    NextEntry:=QueueEntry.Next;
    if PrevEntry = nil then
     begin
      QueueTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Queue Count}
    Dec(QueueTableCount);

    {Release the Lock (Done locally due to invalidate above)}
    if (QueueEntry.Flags and QUEUE_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinUnlockIRQFIQ(QueueEntry.Lock);
     end
    else if (QueueEntry.Flags and QUEUE_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinUnlockFIQ(QueueEntry.Lock);
     end
    else if (QueueEntry.Flags and QUEUE_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinUnlockIRQ(QueueEntry.Lock);
     end
    else
     begin
      Result:=SpinUnlock(QueueEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Free Queue Lock}
    SpinDestroy(QueueEntry.Lock);

    {Free Queue Entry}
    FreeMem(QueueEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(QueueTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function QueueCount(Queue:TQueueHandle):LongWord;
{Get the current count from the supplied queue}
{List: Handle of Queue entry to get from}
{Return: Queue count on success or INVALID_HANDLE_VALUE on failure}
var
 QueueEntry:PQueueEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Get Count}
 Result:=QueueEntry.Count;
end;

{==============================================================================}

function QueueEnqueue(Queue:TQueueHandle;Thread:TThreadHandle):LongWord;
{Add the supplied thread as the last item in the Queue}
{Queue: Handle of Queue entry to add to}
{Thread: Handle of the Thread to enqueue}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If queue is a scheduler queue then caller must hold the lock on the thread}
var
 ThreadEntry:PThreadEntry;
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if QueueLock(Queue) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Get Element}
    case QueueEntry.QueueType of
     QUEUE_TYPE_NOT_SPECIFIED:begin
       {Allocate an Element}
       QueueElement:=AllocMem(SizeOf(TQueueElement));
       QueueElement.Thread:=Thread;
       QueueElement.Key:=0;
      end;
     QUEUE_TYPE_SCHEDULE_SLEEP,QUEUE_TYPE_SCHEDULE_TIMEOUT,QUEUE_TYPE_SCHEDULE_TERMINATION:begin
       {Check the Handle}
       ThreadEntry:=PThreadEntry(Thread);
       if ThreadEntry = nil then Exit;
       if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

       {Use Thread Element}
       QueueElement:=@ThreadEntry.QueueElement;
       QueueElement.Key:=0;

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=Queue;
      end;
     QUEUE_TYPE_SCHEDULE_NONE,QUEUE_TYPE_SCHEDULE_IDLE,QUEUE_TYPE_SCHEDULE_LOWEST,QUEUE_TYPE_SCHEDULE_LOWER,
     QUEUE_TYPE_SCHEDULE_NORMAL,QUEUE_TYPE_SCHEDULE_HIGHER,QUEUE_TYPE_SCHEDULE_HIGHEST,QUEUE_TYPE_SCHEDULE_CRITICAL:begin
       {Check the Handle}
       ThreadEntry:=PThreadEntry(Thread);
       if ThreadEntry = nil then Exit;
       if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

       {Use Thread Element}
       QueueElement:=@ThreadEntry.QueueElement;
       QueueElement.Key:=0;

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=Queue;

       {Increment Thread Count}
       InterlockedIncrement(LongInt(SchedulerThreadCount[ThreadEntry.CurrentCPU]));

       {Update Priority Mask}
       if QueueEntry.Count = 0 then InterlockedOr(LongInt(SchedulerPriorityMask[ThreadEntry.CurrentCPU]),SCHEDULER_MASKS[ThreadEntry.Priority]);
      end;
    end;

    {Check Last}
    if QueueEntry.Last = nil then
     begin
      QueueElement.Next:=nil;
      QueueElement.Prev:=nil;
      QueueEntry.First:=QueueElement;
      QueueEntry.Last:=QueueElement;
     end
    else
     begin
      QueueElement.Next:=nil;
      QueueElement.Prev:=QueueEntry.Last;
      QueueEntry.Last.Next:=QueueElement;
      QueueEntry.Last:=QueueElement;
     end;

    {Increment Count}
    Inc(QueueEntry.Count);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock the Queue}
    QueueUnlock(Queue);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function QueueDequeue(Queue:TQueueHandle):TThreadHandle;
{Get and remove the first thread from the Queue}
{Queue: Handle of Queue entry to get from}
{Return: Handle of dequeued Thread or INVALID_HANDLE_VALUE on failure}
begin
 {}
 Result:=QueueDequeueEx(Queue,True,True);
end;

{==============================================================================}

function QueueDequeueEx(Queue:TQueueHandle;Lock,Unlock:Boolean):TThreadHandle;
{Get and remove the first thread from the Queue}
{Queue: Handle of Queue entry to get from}
{Lock: Lock the Queue on entry if True}
{Unlock: Unlock the Queue on exit if True}
{Return: Handle of dequeued Thread or INVALID_HANDLE_VALUE on failure}
{Note: Extended version used internally by the scheduler}
var
 ThreadEntry:PThreadEntry;
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if not(Lock) or (QueueLock(Queue) = ERROR_SUCCESS) then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Get Element (First)}
    QueueElement:=QueueEntry.First;
    if QueueElement <> nil then
     begin
      {Remove First}
      QueueEntry.First:=QueueElement.Next;

      {Check Next}
      if QueueElement.Next = nil then
       begin
        QueueEntry.Last:=nil;
       end
      else
       begin
        QueueElement.Next.Prev:=nil;
       end;

      {Check Flags}
      if (QueueEntry.Flags and QUEUE_FLAG_DELTA) <> 0 then
       begin
        if (QueueEntry.Flags and QUEUE_FLAG_DESCENDING) = 0 then
         begin
          {Check First}
          if QueueEntry.First <> nil then
           begin
            {Update Key}
            Inc(QueueEntry.First.Key,QueueElement.Key);
           end;
         end;
       end;

      {Decrement Count}
      Dec(QueueEntry.Count);

      {Return Result}
      Result:=QueueElement.Thread;

      {Release Element}
      case QueueEntry.QueueType of
       QUEUE_TYPE_NOT_SPECIFIED:begin
         {Free Element}
         FreeMem(QueueElement);
        end;
       QUEUE_TYPE_SCHEDULE_SLEEP,QUEUE_TYPE_SCHEDULE_TIMEOUT,QUEUE_TYPE_SCHEDULE_TERMINATION:begin
         {Note: No lock held on the thread but it can only be in one scheduler queue at a time}
         {Check Thread}
         if Result = INVALID_HANDLE_VALUE then Exit;

         {Check the Handle}
         ThreadEntry:=PThreadEntry(Result);
         if ThreadEntry = nil then Exit;
         if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

         {Set ScheduleQueue}
         ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
        end;
       QUEUE_TYPE_SCHEDULE_NONE,QUEUE_TYPE_SCHEDULE_IDLE,QUEUE_TYPE_SCHEDULE_LOWEST,QUEUE_TYPE_SCHEDULE_LOWER,
       QUEUE_TYPE_SCHEDULE_NORMAL,QUEUE_TYPE_SCHEDULE_HIGHER,QUEUE_TYPE_SCHEDULE_HIGHEST,QUEUE_TYPE_SCHEDULE_CRITICAL:begin
         {Note: No lock held on the thread but it can only be in one scheduler queue at a time}
         {Check Thread}
         if Result = INVALID_HANDLE_VALUE then Exit;

         {Check the Handle}
         ThreadEntry:=PThreadEntry(Result);
         if ThreadEntry = nil then Exit;
         if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

         {Set ScheduleQueue}
         ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;

         {Decrement Thread Count}
         InterlockedDecrement(LongInt(SchedulerThreadCount[ThreadEntry.CurrentCPU]));

         {Update Priority Mask}
         if QueueEntry.Count = 0 then InterlockedAnd(LongInt(SchedulerPriorityMask[ThreadEntry.CurrentCPU]),not(SCHEDULER_MASKS[ThreadEntry.Priority]));
        end;
      end;
     end;
   finally
    {Unlock the Queue}
    if Unlock then QueueUnlock(Queue);
   end;
  end;
end;

{==============================================================================}

function QueueFirstKey(Queue:TQueueHandle):Integer;
{Get the first Key value from the Queue}
{Queue: Handle of Queue entry to get from}
{Return: First Key value from queue or QUEUE_KEY_NONE on failure}
begin
 {}
 Result:=QueueFirstKeyEx(Queue,True,True);
end;

{==============================================================================}

function QueueFirstKeyEx(Queue:TQueueHandle;Lock,Unlock:Boolean):Integer;
{Get the first Key value from the Queue}
{Queue: Handle of Queue entry to get from}
{Lock: Lock the Queue on entry if True}
{Unlock: Unlock the Queue on exit if True}
{Return: First Key value from queue or QUEUE_KEY_NONE on failure}
{Note: Extended version used internally by the scheduler}
var
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
begin
 {}
 Result:=QUEUE_KEY_NONE;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if not(Lock) or (QueueLock(Queue) = ERROR_SUCCESS) then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Get Element (First)}
    QueueElement:=QueueEntry.First;
    if QueueElement <> nil then
     begin
      {Return Key}
      Result:=QueueElement.Key;
     end;
   finally
    {Unlock the Queue}
    if Unlock then QueueUnlock(Queue);
   end;
  end;
end;

{==============================================================================}

function QueueLastKey(Queue:TQueueHandle):Integer;
{Get the last Key value from the Queue}
{Queue: Handle of Queue entry to get from}
{Return: Last Key value from queue or QUEUE_KEY_NONE on failure}
begin
 {}
 Result:=QueueLastKeyEx(Queue,True,True);
end;

{==============================================================================}

function QueueLastKeyEx(Queue:TQueueHandle;Lock,Unlock:Boolean):Integer;
{Get the last Key value from the Queue}
{Queue: Handle of Queue entry to get from}
{Lock: Lock the Queue on entry if True}
{Unlock: Unlock the Queue on exit if True}
{Return: Last Key value from queue or QUEUE_KEY_NONE on failure}
{Note: Extended version used internally by the scheduler}
var
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
begin
 {}
 Result:=QUEUE_KEY_NONE;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if not(Lock) or (QueueLock(Queue) = ERROR_SUCCESS) then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Get Element (Last)}
    QueueElement:=QueueEntry.Last;
    if QueueElement <> nil then
     begin
      {Return Key}
      Result:=QueueElement.Key;
     end;
   finally
    {Unlock the Queue}
    if Unlock then QueueUnlock(Queue);
   end;
  end;
end;

{==============================================================================}

function QueueInsertKey(Queue:TQueueHandle;Thread:TThreadHandle;Key:Integer):LongWord;
{Insert the supplied thread in the Queue ordered based on Key and the flags of the Queue}
{Queue: Handle of Queue entry to insert into}
{Thread: Handle of thread to be inserted}
{Key: The key to order the insertion on}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If queue is a scheduler queue then caller must hold the lock on the thread}
var
 Delta:Boolean;
 Offset:Integer;
 ThreadEntry:PThreadEntry;
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
 PrevElement:PQueueElement;
 NextElement:PQueueElement;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if QueueLock(Queue) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Get Element}
    case QueueEntry.QueueType of
     QUEUE_TYPE_NOT_SPECIFIED:begin
       {Allocate an Element}
       QueueElement:=AllocMem(SizeOf(TQueueElement));
       QueueElement.Thread:=Thread;
       QueueElement.Key:=0;
      end;
     QUEUE_TYPE_SCHEDULE_SLEEP,QUEUE_TYPE_SCHEDULE_TIMEOUT,QUEUE_TYPE_SCHEDULE_TERMINATION:begin
       {Check the Handle}
       ThreadEntry:=PThreadEntry(Thread);
       if ThreadEntry = nil then Exit;
       if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

       {Use Thread Element}
       QueueElement:=@ThreadEntry.QueueElement;
       QueueElement.Key:=0;

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=Queue;
      end;
     QUEUE_TYPE_SCHEDULE_NONE,QUEUE_TYPE_SCHEDULE_IDLE,QUEUE_TYPE_SCHEDULE_LOWEST,QUEUE_TYPE_SCHEDULE_LOWER,
     QUEUE_TYPE_SCHEDULE_NORMAL,QUEUE_TYPE_SCHEDULE_HIGHER,QUEUE_TYPE_SCHEDULE_HIGHEST,QUEUE_TYPE_SCHEDULE_CRITICAL:begin
       {Check the Handle}
       ThreadEntry:=PThreadEntry(Thread);
       if ThreadEntry = nil then Exit;
       if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

       {Use Thread Element}
       QueueElement:=@ThreadEntry.QueueElement;
       QueueElement.Key:=0;

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=Queue;

       {Increment Thread Count}
       InterlockedIncrement(LongInt(SchedulerThreadCount[ThreadEntry.CurrentCPU]));

       {Update Priority Mask}
       if QueueEntry.Count = 0 then InterlockedOr(LongInt(SchedulerPriorityMask[ThreadEntry.CurrentCPU]),SCHEDULER_MASKS[ThreadEntry.Priority]);
      end;
    end;

    {Find Element}
    Delta:=((QueueEntry.Flags and QUEUE_FLAG_DELTA) <> 0);
    Offset:=0;
    PrevElement:=nil;
    NextElement:=QueueEntry.First;
    if (QueueEntry.Flags and QUEUE_FLAG_DESCENDING) <> 0 then
     begin
      while NextElement <> nil do
       begin
        if Delta then
         begin
          {Delta Descending}
          //To Do //Wrong //Critical //Don't allow Delta Descending ?
          Inc(Offset,NextElement.Key);
          if Key > Offset then
           begin
            //To Do //Wrong //Critical //Don't allow Delta Descending ?
            Break;
           end;
         end
        else
         begin
          {Descending}
          if Key > NextElement.Key then
           begin
            Break;
           end;
         end;
        PrevElement:=NextElement;
        NextElement:=NextElement.Next;
       end;
     end
    else
     begin
      while NextElement <> nil do
       begin
        if Delta then
         begin
          {Delta Ascending}
          if Key < (Offset + NextElement.Key) then
           begin
            Dec(NextElement.Key,(Key - Offset));
            Break;
           end;
          Inc(Offset,NextElement.Key);
         end
        else
         begin
          {Ascending}
          if Key < NextElement.Key then
           begin
            Break;
           end;
         end;
        PrevElement:=NextElement;
        NextElement:=NextElement.Next;
       end;
     end;

    {Insert Element}
    QueueElement.Key:=(Key - Offset);
    {Get Prev/Next}
    QueueElement.Prev:=PrevElement;
    QueueElement.Next:=NextElement;
    {Check Prev}
    if PrevElement = nil then
     begin
      QueueEntry.First:=QueueElement;
      {Check Next}
      if NextElement = nil then
       begin
        QueueEntry.Last:=QueueElement;
       end
      else
       begin
        NextElement.Prev:=QueueElement;
       end;
     end
    else
     begin
      PrevElement.Next:=QueueElement;
      {Check Next}
      if NextElement = nil then
       begin
        QueueEntry.Last:=QueueElement;
       end
      else
       begin
        NextElement.Prev:=QueueElement;
       end;
     end;

    {Increment Count}
    Inc(QueueEntry.Count);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock the Queue}
    QueueUnlock(Queue);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function QueueDeleteKey(Queue:TQueueHandle;Thread:TThreadHandle):LongWord;
{Delete the supplied thread from the Queue based on the flags of the Queue}
{Queue: Handle of Queue entry to delete from}
{Thread: Handle of thread to be deleted}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: If queue is a scheduler queue then caller must hold the lock on the thread}
var
 ThreadEntry:PThreadEntry;
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
 PrevElement:PQueueElement;
 NextElement:PQueueElement;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if QueueLock(Queue) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Find Element}
    QueueElement:=QueueEntry.First;
    while QueueElement <> nil do
     begin
      if QueueElement.Thread = Thread then
       begin
        Break;
       end;
      QueueElement:=QueueElement.Next;
     end;

    {Check Element}
    if QueueElement <> nil then
     begin
      {Delete Element}
      {Get Prev/Next}
      PrevElement:=QueueElement.Prev;
      NextElement:=QueueElement.Next;
      {Check Prev}
      if PrevElement = nil then
       begin
        QueueEntry.First:=NextElement;
        {Check Next}
        if NextElement = nil then
         begin
          QueueEntry.Last:=nil;
         end
        else
         begin
          NextElement.Prev:=nil;
         end;
       end
      else
       begin
        PrevElement.Next:=NextElement;
        {Check Next}
        if NextElement = nil then
         begin
          QueueEntry.Last:=PrevElement;
         end
        else
         begin
          NextElement.Prev:=PrevElement;
         end;
       end;

      {Check Flags}
      if (QueueEntry.Flags and QUEUE_FLAG_DELTA) <> 0 then
       begin
        if (QueueEntry.Flags and QUEUE_FLAG_DESCENDING) <> 0 then
         begin
          //To Do //Critical //Don't allow Delta Descending ?
          {Check Next}
          if NextElement <> nil then
           begin
            {Update Key}
            Inc(NextElement.Key,QueueElement.Key);
           end;
         end
        else
         begin
          {Check Next}
          if NextElement <> nil then
           begin
            {Update Key}
            Inc(NextElement.Key,QueueElement.Key);
           end;
         end;
       end;

      {Decrement Count}
      Dec(QueueEntry.Count);

      {Release Element}
      case QueueEntry.QueueType of
       QUEUE_TYPE_NOT_SPECIFIED:begin
         {Free Element}
         FreeMem(QueueElement);
        end;
       QUEUE_TYPE_SCHEDULE_SLEEP,QUEUE_TYPE_SCHEDULE_TIMEOUT,QUEUE_TYPE_SCHEDULE_TERMINATION:begin
         {Check the Handle}
         ThreadEntry:=PThreadEntry(Thread);
         if ThreadEntry = nil then Exit;
         if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

         {Set ScheduleQueue}
         ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
        end;
       QUEUE_TYPE_SCHEDULE_NONE,QUEUE_TYPE_SCHEDULE_IDLE,QUEUE_TYPE_SCHEDULE_LOWEST,QUEUE_TYPE_SCHEDULE_LOWER,
       QUEUE_TYPE_SCHEDULE_NORMAL,QUEUE_TYPE_SCHEDULE_HIGHER,QUEUE_TYPE_SCHEDULE_HIGHEST,QUEUE_TYPE_SCHEDULE_CRITICAL:begin
         {Check the Handle}
         ThreadEntry:=PThreadEntry(Thread);
         if ThreadEntry = nil then Exit;
         if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

         {Set ScheduleQueue}
         ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;

         {Decrement Thread Count}
         InterlockedDecrement(LongInt(SchedulerThreadCount[ThreadEntry.CurrentCPU]));

         {Update Priority Mask}
         if QueueEntry.Count = 0 then InterlockedAnd(LongInt(SchedulerPriorityMask[ThreadEntry.CurrentCPU]),not(SCHEDULER_MASKS[ThreadEntry.Priority]));
        end;
      end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     end;
   finally
    {Unlock the Queue}
    QueueUnlock(Queue);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function QueueIncrementKey(Queue:TQueueHandle):Integer;
{Increment the first Key value in the Queue}
{Queue: Handle of Queue entry to increment in}
{Return: First Key value in queue after increment or QUEUE_KEY_NONE on failure}
var
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
begin
 {}
 Result:=QUEUE_KEY_NONE;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if QueueLock(Queue) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Get First Element}
    QueueElement:=QueueEntry.First;
    if QueueElement <> nil then
     begin
      {Increment Key}
      if QueueElement.Key < QUEUE_KEY_MAX then
       begin
        Inc(QueueElement.Key);
       end;

      {Return Result}
      Result:=QueueElement.Key;
     end;
   finally
    {Unlock the Queue}
    QueueUnlock(Queue);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function QueueDecrementKey(Queue:TQueueHandle):Integer;
{Decrement the first Key value in the Queue}
{Queue: Handle of Queue entry to decrement in}
{Return: First Key value in queue after decrement or QUEUE_KEY_NONE on failure}
var
 QueueEntry:PQueueEntry;
 QueueElement:PQueueElement;
begin
 {}
 Result:=QUEUE_KEY_NONE;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if QueueLock(Queue) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Get First Element}
    QueueElement:=QueueEntry.First;
    if QueueElement <> nil then
     begin
      {Decrement Key}
      if QueueElement.Key > QUEUE_KEY_MIN then
       begin
        Dec(QueueElement.Key);
       end;

      {Return Result}
      Result:=QueueElement.Key;
     end;
   finally
    {Unlock the Queue}
    QueueUnlock(Queue);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function QueueIsEmpty(Queue:TQueueHandle):Boolean;
{Check if the supplied Queue is empty}
{Queue: Handle of Queue entry to check}
{Return: True if Queue is empty or does not exist, False if Queue is not empty}
var
 QueueEntry:PQueueEntry;
begin
 {}
 Result:=True; {Default to True}

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if QueueLock(Queue) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Check Empty}
    if QueueEntry.First = nil then Exit;

    Result:=False;
   finally
    {Unlock the Queue}
    QueueUnlock(Queue);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function QueueNotEmpty(Queue:TQueueHandle):Boolean;
{Check if the supplied Queue is not empty}
{Queue: Handle of Queue entry to check}
{Return: True if Queue is not empty, False if Queue is empty or does not exist}
var
 QueueEntry:PQueueEntry;
begin
 {}
 Result:=False;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Lock the Queue}
 if QueueLock(Queue) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

    {Check Empty}
    if QueueEntry.First = nil then Exit;

    Result:=True;
   finally
    {Unlock the Queue}
    QueueUnlock(Queue);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function QueueLock(Queue:TQueueHandle):LongWord; {$IFDEF QUEUE_INLINE}inline;{$ENDIF}
{Lock the supplied Queue}
{Queue: Handle of Queue entry to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 QueueEntry:PQueueEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Acquire the Lock}
 if (QueueEntry.Flags and QUEUE_FLAG_IRQFIQ) <> 0 then
  begin
   Result:=SpinLockIRQFIQ(QueueEntry.Lock);
  end
 else if (QueueEntry.Flags and QUEUE_FLAG_FIQ) <> 0 then
  begin
   Result:=SpinLockFIQ(QueueEntry.Lock);
  end
 else if (QueueEntry.Flags and QUEUE_FLAG_IRQ) <> 0 then
  begin
   Result:=SpinLockIRQ(QueueEntry.Lock);
  end
 else
  begin
   Result:=SpinLock(QueueEntry.Lock);
  end;
end;

{==============================================================================}

function QueueUnlock(Queue:TQueueHandle):LongWord; {$IFDEF QUEUE_INLINE}inline;{$ENDIF}
{Unlock the supplied Queue}
{Queue: Handle of Queue entry to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 QueueEntry:PQueueEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Queue}
 if Queue = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 QueueEntry:=PQueueEntry(Queue);
 if QueueEntry = nil then Exit;
 if QueueEntry.Signature <> QUEUE_SIGNATURE then Exit;

 {Release the Lock}
 if (QueueEntry.Flags and QUEUE_FLAG_IRQFIQ) <> 0 then
  begin
   Result:=SpinUnlockIRQFIQ(QueueEntry.Lock);
  end
 else if (QueueEntry.Flags and QUEUE_FLAG_FIQ) <> 0 then
  begin
   Result:=SpinUnlockFIQ(QueueEntry.Lock);
  end
 else if (QueueEntry.Flags and QUEUE_FLAG_IRQ) <> 0 then
  begin
   Result:=SpinUnlockIRQ(QueueEntry.Lock);
  end
 else
  begin
   Result:=SpinUnlock(QueueEntry.Lock);
  end;
end;

{==============================================================================}
{==============================================================================}
{Thread Functions}
function ThreadCreate(StartProc:TThreadStart;StackSize,Priority:LongWord;Name:PChar;Parameter:Pointer):TThreadHandle; {$IFDEF THREAD_INLINE}inline;{$ENDIF}
{Create and insert a new Thread entry

 The new thread will be created suspended so it will not start running until it is
 scheduled with either ThreadReady or ThreadResume}
{StartProc: Procedure address where the thread will start running}
{StackSize: Stack size in bytes}
{Priority: Thread priority (eg THREAD_PRIORITY_NORMAL)}
{Name: Name of the thread}
{Parameter: Parameter passed to StartProc of new thread}
{Return: Handle of new thread or INVALID_HANDLE_VALUE if a new thread could not be created}
{Note: Calls ThreadCreateEx with:
         Affinity = SCHEDULER_CPU_MASK (Run on any available CPU)
         CPU = SchedulerThreadNext (Assign to next CPU in round robin)}

{WARNING: ThreadCreate and ThreadCreateEx are only used internally by SysBeginThread and SysBeginThreadEx

          These functions do not handle setting up certain RTL functionality such as thread variables,
          exceptions and standard input/output handles.

          If you need to create a standard thread use either BeginThread (or BeginThreadEx) or use the
          TThread class and its descendants. Only use ThreadCreate and ThreadCreateEx if you need to modify
          the thread creation behaviour and understand that you also need to handle the additional RTL setup}
begin
 {}
 Result:=ThreadCreateEx(StartProc,StackSize,Priority,SCHEDULER_CPU_MASK,SchedulerThreadNext,Name,Parameter);
end;

{==============================================================================}

function ThreadCreateEx(StartProc:TThreadStart;StackSize,Priority,Affinity,CPU:LongWord;Name:PChar;Parameter:Pointer):TThreadHandle;
{Create and insert a new Thread entry

 The new thread will be created suspended so it will not start running until it is
 scheduled with either ThreadReady or ThreadResume}
{StartProc: Procedure address where the thread will start running}
{StackSize: Stack size in bytes}
{Priority: Thread priority (eg THREAD_PRIORITY_NORMAL)}
{Affinity: Thread affinity (eg CPU_AFFINITY_ALL)}
{CPU: The CPU to assign new thread to (eg CPU_ID_0)}
{Name: Name of the thread}
{Parameter: Parameter passed to StartProc of new thread}
{Return: Handle of new thread or INVALID_HANDLE_VALUE if a new thread could not be created}

{WARNING: ThreadCreate and ThreadCreateEx are only used internally by SysBeginThread and SysBeginThreadEx

          These functions do not handle setting up certain RTL functionality such as thread variables,
          exceptions and standard input/output handles.

          If you need to create a standard thread use either BeginThread (or BeginThreadEx) or use the
          TThread class or its descendants. Only use ThreadCreate and ThreadCreateEx if you need to modify
          the thread creation behaviour and understand that you also need to handle the additional RTL setup}
var
 Count:LongWord;
 StackBase:Pointer;
 FirstEntry:PThreadEntry;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Create Ex (StackSize=' + IntToStr(StackSize) + ' Priority=' + IntToStr(Priority) + ' Affinity=' + IntToHex(Affinity,8) + ' CPU=' + IntToStr(CPU) + ' Name=' + Name + ')');
 {$ENDIF}

 {Check Stack Size}
 if StackSize < THREAD_STACK_MINIMUM_SIZE then
  begin
   StackSize:=THREAD_STACK_MINIMUM_SIZE;
  end;
 if StackSize > THREAD_STACK_MAXIMUM_SIZE then
  begin
   StackSize:=THREAD_STACK_MAXIMUM_SIZE;
  end;

 {Check Priority}
 if (Priority < THREAD_PRIORITY_MINIMUM) or (Priority > THREAD_PRIORITY_MAXIMUM) then
  begin
   Priority:=THREAD_PRIORITY_DEFAULT;
  end;

 {Check Affinity}
 if (Affinity = CPU_AFFINITY_ALL) or (Affinity = CPU_AFFINITY_NONE) or ((Affinity and not(SCHEDULER_CPU_MASK)) <> 0) then
  begin
   Affinity:=SCHEDULER_CPU_MASK;
  end;

 {Check CPU}
 if CPU > (SCHEDULER_CPU_COUNT - 1) then
  begin
   CPU:=SchedulerThreadNext;
  end;

 {Allocate Thread Stack}
 StackBase:=ThreadAllocateStack(StackSize);
 if StackBase = nil then
  begin
   if THREAD_LOG_ENABLED then ThreadLogError('ThreadCreateEx: Failed to allocate stack (StackSize=' + IntToStr(StackSize) + ')');

   Exit;
  end;

 {Create Thread entry}
 if THREAD_SHARED_MEMORY then
  begin
   ThreadEntry:=AllocSharedMem(SizeOf(TThreadEntry));
  end
 else
  begin
   ThreadEntry:=AllocMem(SizeOf(TThreadEntry));
  end;
 if ThreadEntry = nil then
  begin
   if THREAD_LOG_ENABLED then ThreadLogError('ThreadCreateEx: Failed to allocate thread entry');

   {Release Thread Stack}
   ThreadReleaseStack(StackBase,StackSize);

   Exit;
  end;

 {Setup Thread entry}
 {Thread Properties}
 ThreadEntry.Signature:=THREAD_SIGNATURE;
 ThreadEntry.State:=THREAD_STATE_SUSPENDED;
 ThreadEntry.Flags:=THREAD_FLAG_NONE;
 ThreadEntry.Priority:=Priority;
 ThreadEntry.Affinity:=Affinity;
 ThreadEntry.StackBase:=StackBase;
 ThreadEntry.StackSize:=StackSize;
 StrLCopy(ThreadEntry.Name,Name,THREAD_NAME_LENGTH - 1);
 ThreadEntry.Lock:=SpinCreate;
 ThreadEntry.Parent:=ThreadGetCurrent;
 ThreadEntry.Messages.Maximum:=THREAD_MESSAGES_MAXIMUM;
 ThreadEntry.Messages.List:=AllocMem(SizeOf(TMessage) * ThreadEntry.Messages.Maximum);
 ThreadEntry.TlsPointer:=AllocMem(ThreadVarBlockSize);
 ThreadEntry.ExitCode:=ERROR_SUCCESS;
 ThreadEntry.LastError:=ERROR_SUCCESS;
 ThreadEntry.Locale:=LOCALE_DEFAULT;
 {Internal Properties}
 ThreadEntry.CurrentCPU:=CPU;
 ThreadEntry.StackPointer:=nil;                                                                       {Set by ThreadSetupStack below}
 ThreadEntry.TargetCPU:=CPU;
 ThreadEntry.TargetPriority:=Priority;
 ThreadEntry.List:=ListCreateEx(LIST_TYPE_WAIT_THREAD,SchedulerGetListFlags(LIST_TYPE_WAIT_THREAD));  {INVALID_HANDLE_VALUE;} {Preallocated on threads to prevent IRQ/FIQ deadlocks in ListCreate}
 ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
 ThreadEntry.WaitLists:=nil;
 ThreadEntry.WaitResult:=ERROR_SUCCESS;
 ThreadEntry.ReceiveResult:=ERROR_SUCCESS;
 ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
 ThreadEntry.ListElement.Thread:=TThreadHandle(ThreadEntry);
 ThreadEntry.QueueElement.Thread:=TThreadHandle(ThreadEntry);
 {Statistics Properties}
 ThreadEntry.CreateTime:=ClockGetTime;
 ThreadEntry.ExitTime:=TIME_TICKS_TO_1899;
 ThreadEntry.KernelTime:=TIME_TICKS_TO_1899;
 ThreadEntry.SwitchCount:=0;

 {Setup Thread Stack}
 ThreadEntry.StackPointer:=ThreadSetupStack(StackBase,StartProc,ThreadEnd,Parameter);
 if ThreadEntry.StackPointer = nil then
  begin
   if THREAD_LOG_ENABLED then ThreadLogError('ThreadCreateEx: Failed to setup stack');

   {Release Thread Stack}
   ThreadReleaseStack(StackBase,StackSize);

   {Free Message List}
   FreeMem(ThreadEntry.Messages.List);

   {Free RTL Thread Vars}
   FreeMem(ThreadEntry.TlsPointer);

   {Free Thread List}
   ListDestroy(ThreadEntry.List);

   {Free Thread Lock}
   SpinDestroy(ThreadEntry.Lock);

   {Free Thread Entry}
   FreeMem(ThreadEntry);

   Exit;
  end;

 {Insert Thread entry}
 if SpinLock(ThreadTableLock) = ERROR_SUCCESS then
  begin
   try
    {Set TLS Thread Var Flags}
    FirstEntry:=ThreadTable;
    if FirstEntry <> nil then
     begin
      for Count:=0 to THREAD_TLS_MAXIMUM - 1 do
       begin
        {ThreadEntry.TlsTable[Count]:=nil;} {Cleared by AllocMem}
        ThreadEntry.TlsFlags[Count]:=FirstEntry.TlsFlags[Count];
       end;
     end;

    {Link Thread entry}
    if ThreadTable = nil then
     begin
      ThreadTable:=ThreadEntry;
     end
    else
     begin
      ThreadEntry.Next:=ThreadTable;
      ThreadTable.Prev:=ThreadEntry;
      ThreadTable:=ThreadEntry;
     end;

    {Increment Thread Count}
    Inc(ThreadTableCount);

    {Increment Thread Next}
    SchedulerThreadNext:=(SchedulerThreadNext + 1) mod SCHEDULER_CPU_COUNT;
    while SchedulerThreadAllocation[SchedulerThreadNext] = SCHEDULER_ALLOCATION_DISABLED do
     begin
      SchedulerThreadNext:=(SchedulerThreadNext + 1) mod SCHEDULER_CPU_COUNT;
     end;

    {Return Thread entry}
    Result:=TThreadHandle(ThreadEntry);
   finally
    SpinUnlock(ThreadTableLock);
   end;
  end
 else
  begin
   {Release Thread Stack}
   ThreadReleaseStack(StackBase,StackSize);

   {Free Message List}
   FreeMem(ThreadEntry.Messages.List);

   {Free RTL Thread Vars}
   FreeMem(ThreadEntry.TlsPointer);

   {Free Thread List}
   ListDestroy(ThreadEntry.List);

   {Free Thread Lock}
   SpinDestroy(ThreadEntry.Lock);

   {Free Thread Entry}
   FreeMem(ThreadEntry);
  end;
end;

{==============================================================================}

function ThreadDestroy(Thread:TThreadHandle):LongWord;
{Destroy and remove an existing Thread entry}
{Thread: Handle of thread to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Unlock:Boolean;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
 PrevEntry:PThreadEntry;
 NextEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Destroy (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check the Handle}
 if Thread = IRQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = FIQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = SWI_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = IDLE_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;

 {Check State}
 if (ThreadEntry.State <> THREAD_STATE_TERMINATED) then Exit;

 {Remove Thread entry}
 if SpinLock(ThreadTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
     end;
    if ResultCode = ERROR_SUCCESS then
     begin
      Unlock:=True;
      try
       {Check Signature}
       if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

       {Check for Queue}
       if ThreadEntry.ScheduleQueue <> INVALID_HANDLE_VALUE then
        begin
         {Remove from Queue (Scheduler Termination)}
         Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
         if Result <> ERROR_SUCCESS then Exit;

         {Set ScheduleQueue}
         ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
        end;

       {Check Stack Base}
       if ThreadEntry.StackBase <> nil then
        begin
         {Insert in Queue}
         Result:=QueueInsertKey(SchedulerGetQueueHandle(ThreadEntry.CurrentCPU,QUEUE_TYPE_SCHEDULE_TERMINATION),Thread,SCHEDULER_TERMINATION_QUANTUM);
         if Result <> ERROR_SUCCESS then Exit;

         {Release the Lock}
         if SCHEDULER_FIQ_ENABLED then
          begin
           Result:=SpinUnlockIRQFIQ(ThreadEntry.Lock);
          end
         else
          begin
           Result:=SpinUnlockIRQ(ThreadEntry.Lock);
          end;
         if Result <> ERROR_SUCCESS then Exit;
         Unlock:=False;

         {Release Thread Stack}
         ThreadReleaseStack(ThreadEntry.StackBase,ThreadEntry.StackSize);

         {Clear Stack Base and Size}
         ThreadEntry.StackBase:=nil;
         ThreadEntry.StackSize:=0;

         {Clear Stack Pointer}
         ThreadEntry.StackPointer:=nil;
        end
       else
        begin
         {Check Flags}
         if (ThreadEntry.Flags and THREAD_FLAG_PERSIST) = 0 then
          begin
           {Invalidate Thread entry}
           ThreadEntry.Signature:=0;

           {Unlink Thread entry}
           PrevEntry:=ThreadEntry.Prev;
           NextEntry:=ThreadEntry.Next;
           if PrevEntry = nil then
            begin
             ThreadTable:=NextEntry;
             if NextEntry <> nil then
              begin
               NextEntry.Prev:=nil;
              end;
            end
           else
            begin
             PrevEntry.Next:=NextEntry;
             if NextEntry <> nil then
              begin
               NextEntry.Prev:=PrevEntry;
              end;
            end;

           {Decrement Thread Count}
           Dec(ThreadTableCount);

           {Check Thread Count}
           if ThreadTableCount < 1 then
            begin
             {$IFDEF THREAD_DEBUG}
             if THREAD_LOG_ENABLED then ThreadLogDebug('Final thread exiting, System Halted');
             {$ENDIF}

             Halt;
            end;

           {Release the Lock}
           if SCHEDULER_FIQ_ENABLED then
            begin
             Result:=SpinUnlockIRQFIQ(ThreadEntry.Lock);
            end
           else
            begin
             Result:=SpinUnlockIRQ(ThreadEntry.Lock);
            end;
           if Result <> ERROR_SUCCESS then Exit;
           Unlock:=False;

           {Free Thread List}
           if ThreadEntry.List <> INVALID_HANDLE_VALUE then
            begin
             ListDestroy(ThreadEntry.List);
            end;

           {Release Thread Stack}
           {ThreadReleaseStack(ThreadEntry.StackBase,ThreadEntry.StackSize);} {Released during first call}

           {Free Message List}
           FreeMem(ThreadEntry.Messages.List);

           {Free RTL Thread Vars}
           FreeMem(ThreadEntry.TlsPointer);

           {Free TLS Thread Vars}
           for Count:=0 to THREAD_TLS_MAXIMUM - 1 do
            begin
             if ((ThreadEntry.TlsFlags[Count] and THREAD_TLS_FLAG_FREE) <> 0) and (ThreadEntry.TlsTable[Count] <> nil) then
              begin
               FreeMem(ThreadEntry.TlsTable[Count]);
              end;

             {ThreadEntry.TlsTable[Count]:=nil;} {Cleared by FreeMem}
             {ThreadEntry.TlsFlags[Count]:=THREAD_TLS_FLAG_NONE;} {Cleared by FreeMem}
            end;

           {Free Thread Lock}
           SpinDestroy(ThreadEntry.Lock);

           {Free Thread Entry}
           FreeMem(ThreadEntry);
          end;
        end;

       {Return Result}
       Result:=ERROR_SUCCESS;
      finally
       {Release the Lock}
       if Unlock then
        begin
         if SCHEDULER_FIQ_ENABLED then
          begin
           SpinUnlockIRQFIQ(ThreadEntry.Lock);
          end
         else
          begin
           SpinUnlockIRQ(ThreadEntry.Lock);
          end;
        end;
      end;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;
   finally
    SpinUnlock(ThreadTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadGetCurrent:TThreadHandle; {$IFDEF THREAD_INLINE}inline;{$ENDIF}
{Get the Handle of currently executing thread}
{Return: Thread handle of the currently running thread}
begin
 {}
 if Assigned(ThreadGetCurrentHandler) then
  begin
   Result:=ThreadGetCurrentHandler;
  end
 else
  begin
   Result:=INVALID_HANDLE_VALUE;
  end;
end;

{==============================================================================}

function ThreadSetCurrent(Thread:TThreadHandle):LongWord; {$IFDEF THREAD_INLINE}inline;{$ENDIF}
{Set the Handle of currently executing thread}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Must not be called except during initialization}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Current (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check Thread}
 Result:=ERROR_FUNCTION_FAILED;
 if SCHEDULER_FIQ_ENABLED then
  begin
   if (ThreadGetCurrent <> INVALID_HANDLE_VALUE) and (ThreadGetCurrent <> FIQ_THREAD_HANDLE[CPUGetCurrent]) then Exit;
  end
 else
  begin
   if (ThreadGetCurrent <> INVALID_HANDLE_VALUE) and (ThreadGetCurrent <> IRQ_THREAD_HANDLE[CPUGetCurrent]) then Exit;
  end;

 {Return Result}
 if Assigned(ThreadSetCurrentHandler) then
  begin
   Result:=ThreadSetCurrentHandler(Thread);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ThreadGetName(Thread:TThreadHandle):String;
{Get the name of a Thread}
{Thread: Handle of thread to get}
{Return: Name of thread or empty string on failure}
var
 WorkBuffer:String;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:='';

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Name (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Allocate Buffer}
 SetLength(WorkBuffer,THREAD_NAME_LENGTH - 1);

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Copy to Buffer}
    StrLCopy(PChar(WorkBuffer),ThreadEntry.Name,THREAD_NAME_LENGTH - 1);

    {Result must be returned outside lock}
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;

   {Update Buffer}
   SetLength(WorkBuffer,StrLen(PChar(WorkBuffer)));

   {Return Result}
   Result:=WorkBuffer;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ThreadSetName(Thread:TThreadHandle;const Name:String):LongWord;
{Set the name of a Thread}
{Thread: Handle of thread to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Name (Handle=' + HandleToHex(Thread) + ' Name=' + Name + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Set Name}
    StrLCopy(ThreadEntry.Name,PChar(Name),THREAD_NAME_LENGTH - 1);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadGetCPU(Thread:TThreadHandle):LongWord;
{Get the current CPU of a thread (eg CPU_ID_0)}
{Thread: Handle of thread to get}
{Return: CPU of the thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get CPU (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return CPU}
 Result:=ThreadEntry.CurrentCPU;
end;

{==============================================================================}

function ThreadSetCPU(Thread:TThreadHandle;CPU:LongWord):LongWord;
{Set the current CPU of a thread (eg CPU_ID_0)

 The new CPU will not take affect until the thread is next rescheduled}
{Thread: Handle of thread to set}
{CPU: New thread CPU (eg CPU_ID_0)}
{Return: Previous CPU of thread or INVALID_HANDLE_VALUE on failure}
var
 Current:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set CPU (Handle=' + HandleToHex(Thread) + ' CPU=' + CPUIDToString(CPU) + ')');
 {$ENDIF}

 {Check CPU}
 if CPU > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Check Affinity}
    if (ThreadEntry.Affinity and (1 shl CPU)) = 0 then Exit;

    {Get Current CPU}
    Current:=ThreadEntry.CurrentCPU;

    {Set Target CPU}
    ThreadEntry.TargetCPU:=CPU;

    {Return Result}
    Result:=Current;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ThreadGetState(Thread:TThreadHandle):LongWord;
{Get the current state of a thread (eg THREAD_STATE_SUSPENDED)}
{Thread: Handle of thread to get}
{Return: State of the thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get State (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return State}
 Result:=ThreadEntry.State;
end;

{==============================================================================}

function ThreadGetFlags(Thread:TThreadHandle):LongWord;
{Get the current flags of a thread}
{Thread: Handle of thread to get}
{Return: Flags of the thread (eg THREAD_FLAG_PERSIST) or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Flags (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Flags}
 Result:=ThreadEntry.Flags;
end;

{==============================================================================}

function ThreadSetFlags(Thread:TThreadHandle;Flags:LongWord):LongWord;
{Set the current flags of a thread}
{Thread: Handle of thread to set}
{Flags: Flags to set (eg THREAD_FLAG_PERSIST)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Flags (Handle=' + HandleToHex(Thread) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Flags}
 if (Flags and THREAD_FLAG_INTERNAL) <> 0 then Exit;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Set Flags}
    ThreadEntry.Flags:=Flags;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadAddFlags(Thread:TThreadHandle;Flags:LongWord):LongWord;
{Add flags to the current flags of a thread}
{Thread: Handle of thread to add flags for}
{Flags: Flags to add (eg THREAD_FLAG_PERSIST)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Add Flags (Handle=' + HandleToHex(Thread) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Flags}
 if (Flags and THREAD_FLAG_INTERNAL) <> 0 then Exit;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Add Flags}
    ThreadEntry.Flags:=ThreadEntry.Flags or Flags;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadRemoveFlags(Thread:TThreadHandle;Flags:LongWord):LongWord;
{Remove flags from the current flags of a thread}
{Thread: Handle of thread to remove flags from}
{Flags: Flags to remove (eg THREAD_FLAG_PERSIST)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Remove Flags (Handle=' + HandleToHex(Thread) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Flags}
 if (Flags and THREAD_FLAG_INTERNAL) <> 0 then Exit;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Remove Flags}
    ThreadEntry.Flags:=ThreadEntry.Flags and not(Flags);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadGetLocale(Thread:TThreadHandle):LCID;
{Get the current locale of a thread}
{Thread: Handle of thread to get}
{Return: Locale of the thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Locale (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Locale}
 Result:=ThreadEntry.Locale;
end;

{==============================================================================}

function ThreadSetLocale(Thread:TThreadHandle;Locale:LCID):LongWord;
{Set the locale of a thread}
{Thread: Handle of thread to set}
{Locale: Locale id to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Locale (Handle=' + HandleToHex(Thread) + ' Locale=' + IntToHex(Locale,8) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Set Locale}
    ThreadEntry.Locale:=Locale;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadGetTimes(Thread:TThreadHandle;var CreateTime,ExitTime,KernelTime:Int64):LongWord;
{Get the current times of a thread}
{Thread: Handle of thread to get}
{CreateTime: Buffer to receive the CreateTime value}
{ExitTime: Buffer to receive the ExitTime value}
{KernelTime: Buffer to receive the KernelTime value}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Times (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Get Times}
    CreateTime:=ThreadEntry.CreateTime;
    ExitTime:=ThreadEntry.ExitTime;
    KernelTime:=ThreadEntry.KernelTime;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadGetSwitchCount(Thread:TThreadHandle;var SwitchCount:Int64):LongWord;
{Get the current context switch count of a thread (How many times the thread has been scheduled)}
{Thread: Handle of thread to get}
{SwitchCount: Buffer to receive the SwitchCount value}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Switch Count (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Get Count}
    SwitchCount:=ThreadEntry.SwitchCount;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadGetStackFree:LongWord;
{Get the free stack size of the current thread}
{Return: Free stack size of the current thread or 0 on error}
{Note: No lock required as only ever called by the thread itself}
var
 StackBase:PtrUInt;
 StackSize:LongWord;
 StackPointer:PtrUInt;
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=0;

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Get Size and Base}
 StackBase:=PtrUInt(ThreadEntry.StackBase);
 StackSize:=ThreadEntry.StackSize;

 {Get Stack Pointer}
 StackPointer:=GetSP;

 {Return Stack Free}
 Result:=StackPointer - (StackBase - StackSize);
end;

{==============================================================================}

function ThreadGetStackSize(Thread:TThreadHandle):LongWord;
{Get the current stack size of a thread}
{Thread: Handle of thread to get}
{Return: Stack size of the thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Stack Size (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Stack Size}
 Result:=ThreadEntry.StackSize;
end;

{==============================================================================}

function ThreadGetStackBase(Thread:TThreadHandle):PtrUInt;
{Get the current stack base of a thread}
{Thread: Handle of thread to get}
{Return: Stack base of the thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=PtrUInt(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Stack Base (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Stack Base}
 Result:=PtrUInt(ThreadEntry.StackBase);
end;

{==============================================================================}

function ThreadSetStackBase(Thread:TThreadHandle;StackBase:PtrUInt):LongWord;
{Set the current stack base of a thread}
{Thread: Handle of thread to set}
{StackBase: Stack base to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Must not be called except during initialization}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Stack Base (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check Thread}
 Result:=ERROR_FUNCTION_FAILED;
 if SCHEDULER_FIQ_ENABLED then
  begin
   if ThreadGetCurrent <> FIQ_THREAD_HANDLE[CPUGetCurrent] then Exit;
  end
 else
  begin
   if ThreadGetCurrent <> IRQ_THREAD_HANDLE[CPUGetCurrent] then Exit;
  end;

 {Set Stack Base}
 ThreadEntry.StackBase:=Pointer(StackBase);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ThreadGetStackPointer(Thread:TThreadHandle):PtrUInt;
{Get the current stack pointer of a thread}
{Thread: Handle of thread to get}
{Return: Stack pointer of the thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=PtrUInt(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Stack Pointer (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Stack Pointer}
 Result:=PtrUInt(ThreadEntry.StackPointer);
end;

{==============================================================================}

function ThreadGetExitCode(Thread:TThreadHandle):LongWord;
{Get the exit code of a Thread}
{Thread: Handle of thread to get}
{Return: Exit code of thread, STILL_ACTIVE if the thread has not terminated or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Exit Code (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check State}
 Result:=STILL_ACTIVE;
 if ThreadEntry.State = THREAD_STATE_TERMINATED then
  begin
   {Return ExitCode}
   Result:=ThreadEntry.ExitCode;
  end;
end;

{==============================================================================}

function ThreadGetAffinity(Thread:TThreadHandle):LongWord;
{Get the scheduling affinity of a Thread}
{Thread: Handle of thread to get}
{Return: Affinity of thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Affinity (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Affinity}
 Result:=ThreadEntry.Affinity;
end;

{==============================================================================}

function ThreadSetAffinity(Thread:TThreadHandle;Affinity:LongWord):LongWord;
{Set the scheduling affinity of a Thread

 The new affinity will not take affect until the thread is next rescheduled}
{Thread: Handle of thread to set}
{Affinity: New thread affinity (eg CPU_AFFINITY_0)}
{Return: Previous affinity of thread or INVALID_HANDLE_VALUE on failure}
var
 Current:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Affinity (Handle=' + HandleToHex(Thread) + ' Affinity=' + IntToHex(Affinity,8) + ')');
 {$ENDIF}

 {Check Affinity}
 if (Affinity <> CPU_AFFINITY_ALL) and ((Affinity = CPU_AFFINITY_NONE) or ((Affinity and not(SCHEDULER_CPU_MASK)) <> 0)) then
  begin
   Exit;
  end;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Get Affinity}
    Current:=ThreadEntry.Affinity;

    {Set Affinity}
    ThreadEntry.Affinity:=(Affinity and SCHEDULER_CPU_MASK);

    {Return Result}
    Result:=Current;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ThreadGetPriority(Thread:TThreadHandle):LongWord;
{Get the scheduling priority of a Thread}
{Thread: Handle of thread to get}
{Return: Priority of thread or INVALID_HANDLE_VALUE on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Priority (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Current Priority}
 Result:=ThreadEntry.Priority;
end;

{==============================================================================}

function ThreadSetPriority(Thread:TThreadHandle;Priority:LongWord):LongWord;
{Set the scheduling priority of a Thread

 The new priority will not take affect until the thread is next rescheduled}
{Thread: Handle of thread to set}
{Priority: New thread priority (eg THREAD_PRIORITY_NORMAL)}
{Return: Previous priority of thread or INVALID_HANDLE_VALUE on failure}
var
 Current:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Priority (Handle=' + HandleToHex(Thread) + ' Priority=' + IntToStr(Priority) + ')');
 {$ENDIF}

 {Check Priority}
 if Priority = THREAD_PRIORITY_NONE then
  begin
   {Special case for IRQ/FIQ/SWI Threads}
   if (Thread <> IRQ_THREAD_HANDLE[CPUGetCurrent]) and (Thread <> FIQ_THREAD_HANDLE[CPUGetCurrent]) and (Thread <> SWI_THREAD_HANDLE[CPUGetCurrent]) then
    begin
     Exit;
    end;
  end
 else
  begin
   if (Priority < THREAD_PRIORITY_MINIMUM) or (Priority > THREAD_PRIORITY_MAXIMUM) then
    begin
     Exit;
    end;
  end;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Get Current Priority}
    Current:=ThreadEntry.Priority;

    {Set Target Priority}
    ThreadEntry.TargetPriority:=Priority;

    {Return Result}
    Result:=Current;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function ThreadGetLastError:LongWord;
{Get the last error value for the current Thread}
{Return: Last Error of thread or ERROR_SUCCESS if no error}
{Note: No lock required as only ever called by the thread itself}
var
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return LastError}
 Result:=ThreadEntry.LastError;
end;

{==============================================================================}

procedure ThreadSetLastError(LastError:LongWord);
{Set the last error value for the current Thread}
{Note: No lock required as only ever called by the thread itself}
var
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Set LastError}
 ThreadEntry.LastError:=LastError;
end;

{==============================================================================}

function ThreadSetLastErrorEx(LastError:LongWord):LongWord;
{Set the last error value for the current Thread}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: No lock required as only ever called by the thread itself}
var
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Set LastError}
 ThreadEntry.LastError:=LastError;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ThreadGetWaitResult:LongWord;
{Get the result of the last wait timeout for the current Thread}
{Return: Result of last wait timeout or ERROR_SUCCESS if no error}
{Note: No lock required as only ever called by the thread itself}
var
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return WaitResult}
 Result:=ThreadEntry.WaitResult;
end;

{==============================================================================}

function ThreadGetReceiveResult:LongWord;
{Get the result of the last receive timeout for the current Thread}
{Return: Result of last receive timeout or ERROR_SUCCESS if no error}
{Note: No lock required as only ever called by the thread itself}
var
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return ReceiveResult}
 Result:=ThreadEntry.ReceiveResult;
end;

{==============================================================================}

function ThreadGetTlsIndex(TlsIndex:LongWord):LongWord;
{Get the current status of a TLS index in the TLS index table}
{TlsIndex: The TLS index to get the status for}
{Return: THREAD_TLS_FREE if unused, THREAD_TLS_USED if in use or THREAD_TLS_INVALID on error}
{Note: No lock required as only ever reads from the table}
begin
 {}
 Result:=THREAD_TLS_INVALID;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Tls Index (Index=' + IntToStr(TlsIndex) + ')');
 {$ENDIF}

 {Check Index}
 if TlsIndex >= THREAD_TLS_MAXIMUM then Exit;

 {Get Index Status}
 Result:=ThreadTlsTable[TlsIndex];
end;

{==============================================================================}

function ThreadAllocTlsIndex:LongWord; inline;
{Allocate a TLS index in the TLS index table}
{Return: Allocated TLS index or TLS_OUT_OF_INDEXES on failure}
begin
 {}
 Result:=ThreadAllocTlsIndexEx(THREAD_TLS_FLAG_NONE);
end;

{==============================================================================}

function ThreadAllocTlsIndexEx(Flags:LongWord):LongWord;
{Allocate a TLS index in the TLS index table}
{Flags: The flags to apply to the TLS index entries (eg THREAD_TLS_FLAG_FREE)}
{Return: Allocated TLS index or TLS_OUT_OF_INDEXES on failure}
var
 Count:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=TLS_OUT_OF_INDEXES;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Alloc Tls Index Ex (Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Lock the Table}
 if SpinLock(ThreadTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Indexes}
    for Count:=0 to THREAD_TLS_MAXIMUM - 1 do
     begin
      if ThreadTlsTable[Count] = THREAD_TLS_FREE then
       begin
        {Initialize Thread Tables}
        ThreadEntry:=ThreadTable;
        while ThreadEntry <> nil do
         begin
          ThreadEntry.TlsTable[Count]:=nil;
          ThreadEntry.TlsFlags[Count]:=Flags;

          ThreadEntry:=ThreadEntry.Next;
         end;

        {Allocate Index}
        ThreadTlsTable[Count]:=THREAD_TLS_USED;

        {Increment Index Count}
        Inc(ThreadTlsTableCount);

        {Return Result}
        Result:=Count;
        Exit;
       end;
     end;
   finally
    SpinUnlock(ThreadTableLock);
   end;
  end;
end;

{==============================================================================}

function ThreadReleaseTlsIndex(TlsIndex:LongWord):LongWord;
{Deallocate a TLS index from the TLS index table}
{TlsIndex: The TLS index to deallocate}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Release Tls Index (Index=' + IntToStr(TlsIndex) + ')');
 {$ENDIF}

 {Check Index}
 if TlsIndex >= THREAD_TLS_MAXIMUM then Exit;

 {Lock the Table}
 if SpinLock(ThreadTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Table}
    if ThreadTlsTable[TlsIndex] <> THREAD_TLS_USED then Exit;

    {Release Index}
    ThreadTlsTable[TlsIndex]:=THREAD_TLS_FREE;

    {Decrement Index Count}
    Dec(ThreadTlsTableCount);

    {Initialize Thread Tables}
    ThreadEntry:=ThreadTable;
    while ThreadEntry <> nil do
     begin
      if ((ThreadEntry.TlsFlags[TlsIndex] and THREAD_TLS_FLAG_FREE) <> 0) and (ThreadEntry.TlsTable[TlsIndex] <> nil) then
       begin
        FreeMem(ThreadEntry.TlsTable[TlsIndex]);
       end;

      ThreadEntry.TlsTable[TlsIndex]:=nil;
      ThreadEntry.TlsFlags[TlsIndex]:=THREAD_TLS_FLAG_NONE;

      ThreadEntry:=ThreadEntry.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(ThreadTableLock);
   end;
  end;
end;

{==============================================================================}

function ThreadGetTlsValue(TlsIndex:LongWord):Pointer;
{Get the pointer associated with the TLS index for the current thread}
{Return: Pointer for the specified TLS index or nil if not set or on error}
{Note: No lock required as only ever called by the thread itself}
var
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=nil;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Tls Value (Index=' + IntToStr(TlsIndex) + ')');
 {$ENDIF}

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check the Index}
 if ThreadGetTlsIndex(TlsIndex) <> THREAD_TLS_USED then Exit;

 {Return Pointer}
 Result:=ThreadEntry.TlsTable[TlsIndex];
end;

{==============================================================================}

function ThreadSetTlsValue(TlsIndex:LongWord;TlsValue:Pointer):LongWord;
{Set the pointer associated with the TLS index for the current thread}
{TlsIndex: The TLS index to get the pointer for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: No lock required as only ever called by the thread itself}
var
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Tls Value (Index=' + IntToStr(TlsIndex) + ' Value=' + PtrToHex(TlsValue) + ')');
 {$ENDIF}

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check the Index}
 if ThreadGetTlsIndex(TlsIndex) <> THREAD_TLS_USED then Exit;

 {Set Pointer}
 ThreadEntry.TlsTable[TlsIndex]:=TlsValue;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ThreadGetTlsPointer(Thread:TThreadHandle):Pointer;
{Get the RTL TLS (Thread Local Storage) pointer of a Thread}
{Thread: Handle of thread to get}
{Return: Pointer to the RTL TLS of thread or nil on failure}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=nil;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Get Tls Pointer (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Return Tls Pointer}
 Result:=ThreadEntry.TlsPointer;
end;

{==============================================================================}

function ThreadSetTlsPointer(Thread:TThreadHandle;TlsPointer:Pointer):LongWord;
{Set the RTL TLS (Thread Local Storage) pointer of a Thread}
{Thread: Handle of thread to set}
{TlsPointer: Pointer value to set (Can be nil to clear the pointer)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Set Tls Pointer (Handle=' + HandleToHex(Thread) + ' TlsPointer=' + PtrToHex(TlsPointer) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Set Tls Pointer}
    ThreadEntry.TlsPointer:=TlsPointer;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadReady(Thread:TThreadHandle;Reschedule:Boolean):LongWord;
{Place the supplied Thread on the ready queue}
{Thread: Handle of thread to make ready}
{Reschedule: If True then call SchedulerReschedule}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: When called by the scheduler, thread has already been removed from the sleep queue}
var
 Count:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Ready (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end
    else if ThreadEntry.State = THREAD_STATE_RUNNING then
     begin
      Exit;
     end;

    {Set State}
    ThreadEntry.State:=THREAD_STATE_READY;

    {Do not update Kernel Time}

    {Check CPU}
    if SCHEDULER_CPU_COUNT > 1 then
     begin
      if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectCPU[CPUGetCurrent]);
        {$ENDIF}

        {Update CPU}
        ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
       end
      else
       begin
        {Check Affinity}
        if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectAffinity[CPUGetCurrent]);
          {$ENDIF}

          {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
          for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
           begin
            if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
             begin
              {Update CPU}
              ThreadEntry.CurrentCPU:=Count;
              ThreadEntry.TargetCPU:=Count;

              Break;
             end;
           end;
         end;
       end;
     end;

    {Check Priority}
    if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
     begin
      {$IFDEF SCHEDULER_DEBUG}
      Inc(SchedulerSelectPriority[CPUGetCurrent]);
      {$ENDIF}

      {Update Priority}
      ThreadEntry.Priority:=ThreadEntry.TargetPriority;
     end;

    {Enqueue Thread (Current CPU and Priority)}
    Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;

   {Check Reschedule}
   if Reschedule then SchedulerReschedule(False);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadTimeout(Thread:TThreadHandle):LongWord;
{Place the supplied Thread on the ready queue after a timeout waiting on a resource}
{Thread: Handle of thread to make ready}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: When called by the scheduler, thread has already been removed from the timeout queue}
var
 Count:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Timeout (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end
    else if ThreadEntry.State = THREAD_STATE_RUNNING then
     begin
      Exit;
     end;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_WAIT_TIMEOUT then
     begin
      {Remove from WaitList (Semaphore/CriticalSection/Event/Thread etc)}
      ListRemove(ThreadEntry.WaitList,@ThreadEntry.ListElement);

      {Set WaitList}
      ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;

      {Set WaitResult}
      ThreadEntry.WaitResult:=WAIT_TIMEOUT;
     end
    else if ThreadEntry.State = THREAD_STATE_RECEIVE_TIMEOUT then
     begin
      {Set ReceiveResult}
      ThreadEntry.ReceiveResult:=WAIT_TIMEOUT;
     end
    else
     begin
      Exit;
     end;

    {Set State}
    ThreadEntry.State:=THREAD_STATE_READY;

    {Do not update Kernel Time}

    {Check CPU}
    if SCHEDULER_CPU_COUNT > 1 then
     begin
      if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectCPU[CPUGetCurrent]);
        {$ENDIF}

        {Update CPU}
        ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
       end
      else
       begin
        {Check Affinity}
        if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectAffinity[CPUGetCurrent]);
          {$ENDIF}

          {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
          for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
           begin
            if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
             begin
              {Update CPU}
              ThreadEntry.CurrentCPU:=Count;
              ThreadEntry.TargetCPU:=Count;

              Break;
             end;
           end;
         end;
       end;
     end;

    {Check Priority}
    if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
     begin
      {$IFDEF SCHEDULER_DEBUG}
      Inc(SchedulerSelectPriority[CPUGetCurrent]);
      {$ENDIF}

      {Update Priority}
      ThreadEntry.Priority:=ThreadEntry.TargetPriority;
     end;

    {Enqueue Thread (Current CPU and Priority)}
    Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadWake(Thread:TThreadHandle):LongWord;
{Remove a thread prematurely from the sleep or timeout queues}
{Thread: Handle of thread to remove}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: A thread that was sleeping will simply return early
       A thread that was waiting or receiving with a timeout will return
       with the error WAIT_TIMEOUT
       A thread that was waiting or receiving with INFINITE timeout will return
       with the error WAIT_ABANDONED}
var
 Count:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Wake (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end
    else if ThreadEntry.State = THREAD_STATE_RUNNING then
     begin
      Exit;
     end;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_SLEEP then
     begin
      {Remove from Queue}
      Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else if ThreadEntry.State = THREAD_STATE_WAIT then
     begin
      {Remove from WaitList (Semaphore/CriticalSection/Event/Thread etc)}
      ListRemove(ThreadEntry.WaitList,@ThreadEntry.ListElement);

      {Set WaitList}
      ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;

      {Set WaitResult}
      ThreadEntry.WaitResult:=WAIT_ABANDONED;
     end
    else if ThreadEntry.State = THREAD_STATE_WAIT_TIMEOUT then
     begin
      {Remove from Queue}
      Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
      if Result <> ERROR_SUCCESS then Exit;

      {Remove from WaitList (Semaphore/CriticalSection/Event/Thread etc)}
      ListRemove(ThreadEntry.WaitList,@ThreadEntry.ListElement);

      {Set WaitList}
      ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;

      {Set WaitResult}
      ThreadEntry.WaitResult:=WAIT_TIMEOUT;
     end
    else if ThreadEntry.State = THREAD_STATE_RECEIVE then
     begin
      {Set ReceiveResult}
      ThreadEntry.ReceiveResult:=WAIT_ABANDONED;
     end
    else if ThreadEntry.State = THREAD_STATE_RECEIVE_TIMEOUT then
     begin
      {Remove from Queue}
      Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
      if Result <> ERROR_SUCCESS then Exit;

      {Set ReceiveResult}
      ThreadEntry.ReceiveResult:=WAIT_TIMEOUT;
     end
    else
     begin
      Exit;
     end;

    {Set State}
    ThreadEntry.State:=THREAD_STATE_READY;

    {Do not update Kernel Time}

    {Check CPU}
    if SCHEDULER_CPU_COUNT > 1 then
     begin
      if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectCPU[CPUGetCurrent]);
        {$ENDIF}

        {Update CPU}
        ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
       end
      else
       begin
        {Check Affinity}
        if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectAffinity[CPUGetCurrent]);
          {$ENDIF}

          {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
          for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
           begin
            if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
             begin
              {Update CPU}
              ThreadEntry.CurrentCPU:=Count;
              ThreadEntry.TargetCPU:=Count;

              Break;
             end;
           end;
         end;
       end;
     end;

    {Check Priority}
    if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
     begin
      {$IFDEF SCHEDULER_DEBUG}
      Inc(SchedulerSelectPriority[CPUGetCurrent]);
      {$ENDIF}

      {Update Priority}
      ThreadEntry.Priority:=ThreadEntry.TargetPriority;
     end;

    {Enqueue Thread (Current CPU and Priority)}
    Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadMigrate(Thread:TThreadHandle;CPU:LongWord):LongWord;
{Migrate a thread to a new CPU}
{Thread: Handle of thread to migrate}
{CPU: New CPU for the thread}
{Return: Previous CPU of thread or INVALID_HANDLE_VALUE on failure}
begin
 {}
 Result:=ThreadSetCPU(Thread,CPU);
end;

{==============================================================================}

procedure ThreadEnd(ExitCode:LongWord);
{Terminate the current Thread}
{ExitCode: The return code of the thread}
begin
 {}
 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread End (ExitCode=' + IntToStr(ExitCode) + ')');
 {$ENDIF}

 ThreadTerminate(ThreadGetCurrent,ExitCode);
end;

{==============================================================================}

function ThreadHalt(ExitCode:LongWord):LongWord;
{Halt the current thread so it will never be rescheduled.

 Normally only called due to an unhandled exception etc so that the thread
 is put to sleep permanently without being terminated}
{ExitCode: The return code of the thread}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ExitTime:Int64;
 Reschedule:Boolean;
 ResultCode:LongWord;
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Halt (ExitCode=' + IntToStr(ExitCode) + ')');
 {$ENDIF}

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Get Exit Time}
 ExitTime:=ClockGetTime;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   Reschedule:=False;
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end;

    {Set ExitTime}
    ThreadEntry.ExitTime:=ExitTime;

    {Set ExitCode}
    ThreadEntry.ExitCode:=ExitCode;

    {Set State}
    ThreadEntry.State:=THREAD_STATE_HALTED;

    {$IFDEF THREAD_STATISTICS}
    {Update Kernel Time}
    if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
     begin
      Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
     end;
    {$ENDIF}

    {Reschedule}
    Reschedule:=True;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;

   {Check Reschedule}
   if Reschedule then SchedulerReschedule(False);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadTerminate(Thread:TThreadHandle;ExitCode:LongWord):LongWord;
{Terminate but do not destroy the supplied Thread

 The terminated thread is placed on the termination queue until any threads
 waiting on it have been released}
{Thread: Handle of thread to terminate}
{ExitCode: The return code of the thread}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Terminating a thread from another thread is not safe unless specific precautions are taken
       to prevent deadlocks
       It is normally safe for a thread to terminate itself as long as it releases any locks it is
       holding that may be required by other threads before terminating}
var
 ExitTime:Int64;
 Reschedule:Boolean;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Terminate (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Get Exit Time}
 ExitTime:=ClockGetTime;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   Reschedule:=False;
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end;

    {Set ExitTime}
    ThreadEntry.ExitTime:=ExitTime;

    {Set ExitCode}
    ThreadEntry.ExitCode:=ExitCode;

    {Check List}
    while ListNotEmpty(ThreadEntry.List) do
     begin
      {Release waiting thread}
      ThreadRelease(ThreadEntry.List);
     end;

    {Check State}
    case ThreadEntry.State of
     THREAD_STATE_RUNNING:begin
       {Reschedule}
       Reschedule:=True;
      end;
     THREAD_STATE_READY:begin
       {Remove from Queue (Scheduler Idle/Normal/Critical etc)}
       QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
      end;
     THREAD_STATE_SLEEP:begin
       {Remove from Queue (Scheduler Sleep)}
       QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
      end;
     THREAD_STATE_SUSPENDED:begin
       {Nothing}
      end;
     THREAD_STATE_WAIT:begin
       {Remove from WaitList (Semaphore/CriticalSection/Event/Thread etc)}
       ListRemove(ThreadEntry.WaitList,@ThreadEntry.ListElement);

       {Set WaitList}
       ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
      end;
     THREAD_STATE_WAIT_TIMEOUT:begin
       {Remove from Queue (Scheduler Timeout)}
       QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;

       {Remove from WaitList (Semaphore/CriticalSection/Event/Thread etc)}
       ListRemove(ThreadEntry.WaitList,@ThreadEntry.ListElement);

       {Set WaitList}
       ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
      end;
     THREAD_STATE_RECEIVE:begin
       {Nothing}
      end;
     THREAD_STATE_RECEIVE_TIMEOUT:begin
       {Remove from Queue (Scheduler Timeout)}
       QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);

       {Set ScheduleQueue}
       ThreadEntry.ScheduleQueue:=INVALID_HANDLE_VALUE;
      end;
    end;

    {Set State}
    ThreadEntry.State:=THREAD_STATE_TERMINATED;

    {$IFDEF THREAD_STATISTICS}
    {Check Current Thread (It is normally considered unsafe to terminate another thread that is running)}
    if Thread = ThreadGetCurrent then
     begin
      {Update Kernel Time}
      if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
       begin
        Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
       end;
     end;
    {$ENDIF}

    {Insert in Queue}
    Result:=QueueInsertKey(SchedulerGetQueueHandle(ThreadEntry.CurrentCPU,QUEUE_TYPE_SCHEDULE_TERMINATION),Thread,SCHEDULER_TERMINATION_INITIAL);
    if Result <> ERROR_SUCCESS then Exit;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;

   {Check Reschedule}
   if Reschedule then SchedulerReschedule(False);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadYield:LongWord;
{Make the current thread yield the processor (Same as ThreadSleep(0))}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Reschedule}
 Result:=SchedulerReschedule(True);
end;

{==============================================================================}

function ThreadSleep(Milliseconds:LongWord):LongWord;
{Place the current thread on the sleep queue for a specified number of milliseconds}
{Milliseconds: Number of milliseconds to sleep}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Ticks:Integer;
 ResultCode:LongWord;
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Calculate Ticks}
 Ticks:=SCHEDULER_INTERRUPTS_PER_MILLISECOND * Milliseconds;

 {Check Ticks}
 if Ticks > 0 then
  begin
   {Get Thread}
   Thread:=ThreadGetCurrent;
   if Thread = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   ThreadEntry:=PThreadEntry(Thread);
   if ThreadEntry = nil then Exit;
   if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

   {Check the Handle}
   if Thread = IRQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
   if Thread = FIQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
   if Thread = SWI_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
   if Thread = IDLE_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;

   {Acquire the Lock}
   if SCHEDULER_FIQ_ENABLED then
    begin
     ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
    end
   else
    begin
     ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
    end;
   if ResultCode = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

      Result:=ERROR_OPERATION_FAILED;

      {Check State}
      if ThreadEntry.State = THREAD_STATE_TERMINATED then
       begin
        Exit;
       end;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_SLEEP;

      {$IFDEF THREAD_STATISTICS}
      {Update Kernel Time}
      if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
       begin
        Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
       end;
      {$ENDIF}

      {Insert in Queue (Current CPU)}
      Result:=QueueInsertKey(SchedulerGetQueueHandle(ThreadEntry.CurrentCPU,QUEUE_TYPE_SCHEDULE_SLEEP),Thread,Ticks);
      if Result <> ERROR_SUCCESS then Exit;
     finally
      {Release the Lock}
      if SCHEDULER_FIQ_ENABLED then
       begin
        SpinUnlockIRQFIQ(ThreadEntry.Lock);
       end
      else
       begin
        SpinUnlockIRQ(ThreadEntry.Lock);
       end;
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;
  end;

 {Reschedule}
 SchedulerReschedule(Ticks = 0);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ThreadWait(List:TListHandle;Lock:TSpinHandle;Flags:LongWord):LongWord;
{Put the current thread into a wait state on the supplied list}
{List: Handle of List entry to put thread into}
{Lock: Handle of Lock to release before going into wait state}
{Flags: Flag to indicate which unlock method to use}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Caller must hold the lock on the synchronisation object containing the list}
begin
 {}
 Result:=ThreadWaitEx(List,Lock,Flags,INFINITE);
end;

{==============================================================================}

function ThreadWaitEx(List:TListHandle;Lock:TSpinHandle;Flags,Timeout:LongWord):LongWord;
{Put the current thread into a wait state with timeout on the supplied list}
{List: Handle of List entry to put thread into}
{Lock: Handle of Lock to release before going into wait state}
{Flags: Flag to indicate which unlock method to use}
{Timeout: Milliseconds to wait before timeout (INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Caller must hold the lock on the synchronisation object containing the list}
var
 Ticks:Integer;
 Reschedule:Boolean;
 ResultCode:LongWord;
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Wait (List=' + HandleToHex(List) + ' Lock=' + HandleToHex(Lock) + ')');
 {$ENDIF}

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   Reschedule:=False;
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end;

    {Check Timeout}
    if Timeout = 0 then
     begin
      {This should not happen as the caller should have used Timeout = 0 as a marker
       to check the resource and return if not available, simply return WAIT_TIMEOUT}

      {Set WaitResult}
      ThreadEntry.WaitResult:=WAIT_TIMEOUT;

      {Return Result}
      Result:=ERROR_WAIT_TIMEOUT; {Could be ERROR_SUCCESS, makes no difference}
     end
    else if Timeout = INFINITE then
     begin
      {Add to List}
      Result:=ListAddLast(List,@ThreadEntry.ListElement);
      if Result <> ERROR_SUCCESS then Exit;

      {Set WaitList}
      ThreadEntry.WaitList:=List;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_WAIT;

      {$IFDEF THREAD_STATISTICS}
      {Update Kernel Time}
      if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
       begin
        Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
       end;
      {$ENDIF}

      {Reschedule}
      Reschedule:=True;

      {Return Result}
      Result:=ERROR_SUCCESS;
     end
    else
     begin
      {Calculate Ticks}
      Ticks:=SCHEDULER_INTERRUPTS_PER_MILLISECOND * Timeout;

      {Add to List}
      Result:=ListAddLast(List,@ThreadEntry.ListElement);
      if Result <> ERROR_SUCCESS then Exit;

      {Set WaitList}
      ThreadEntry.WaitList:=List;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_WAIT_TIMEOUT;

      {$IFDEF THREAD_STATISTICS}
      {Update Kernel Time}
      if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
       begin
        Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
       end;
      {$ENDIF}

      {Insert in Queue (Current CPU)}
      Result:=QueueInsertKey(SchedulerGetQueueHandle(ThreadEntry.CurrentCPU,QUEUE_TYPE_SCHEDULE_TIMEOUT),Thread,Ticks);
      if Result <> ERROR_SUCCESS then Exit;

      {Reschedule}
      Reschedule:=True;

      {Return Result}
      Result:=ERROR_SUCCESS;
     end;

    {Check Lock}
    if Lock <> INVALID_HANDLE_VALUE then
     begin
      {Check Flags}
      if (Flags and LOCK_FLAG_IRQFIQ) <> 0 then
       begin
        {Exchange Masks}
        SpinMaskExchange(ThreadEntry.Lock,Lock); //Remove
        {Check Scheduler}
        if SCHEDULER_FIQ_ENABLED then
         begin
          {Enable IRQ}
          EnableIRQ;  //To Do //Critical //This could be a problem if part of a chain, need a way to test the spin mask for IRQ
         end
        else
         begin
          {Enable FIQ}
          EnableFIQ;  //To Do //Critical //This could be a problem if part of a chain, need a way to test the spin mask for FIQ
         end;
        Result:=SpinUnlockIRQFIQ(Lock);
       end
      else if (Flags and LOCK_FLAG_FIQ) <> 0 then
       begin
        {Check Scheduler}
        if SCHEDULER_FIQ_ENABLED then
         begin
          {Exchange Masks}
          SpinMaskExchange(ThreadEntry.Lock,Lock); //Remove
         end;
        Result:=SpinUnlockFIQ(Lock);
       end
      else if (Flags and LOCK_FLAG_IRQ) <> 0 then
       begin
        {Check Scheduler}
        if not(SCHEDULER_FIQ_ENABLED) then
         begin
          {Exchange Masks}
          SpinMaskExchange(ThreadEntry.Lock,Lock);
         end;
        Result:=SpinUnlockIRQ(Lock);
       end
      else
       begin
        Result:=SpinUnlock(Lock);
       end;
      if Result <> ERROR_SUCCESS then Exit;
     end;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;

   {Check Reschedule}
   if Reschedule then SchedulerReschedule(False);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadRelease(List:TListHandle):LongWord;
{Release the first thread waiting on the supplied list}
{List: Handle of List entry to release thread from}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Caller must hold the lock on the synchronisation object containing the list}
var
 Count:LongWord;
 ResultCode:LongWord;
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
 ListElement:PListElement;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Release (List=' + HandleToHex(List) + ')');
 {$ENDIF}

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Get List First}
 ListElement:=ListGetFirstEx(List,True);
 if ListElement <> nil then
  begin
   {Get Thread}
   Thread:=ListElement.Thread;
   if Thread = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   ThreadEntry:=PThreadEntry(Thread);
   if ThreadEntry = nil then Exit;
   if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

   {Acquire the Lock}
   if SCHEDULER_FIQ_ENABLED then
    begin
     ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
    end
   else
    begin
     ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
    end;
   if ResultCode = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

      Result:=ERROR_OPERATION_FAILED;

      {Check State}
      if ThreadEntry.State = THREAD_STATE_TERMINATED then
       begin
        Exit;
       end
      else if ThreadEntry.State = THREAD_STATE_RUNNING then
       begin
        Exit;
       end;

      {Check State}
      if ThreadEntry.State = THREAD_STATE_WAIT then
       begin
        {Set WaitResult}
        ThreadEntry.WaitResult:=ERROR_SUCCESS;

        {Set WaitList}
        ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
       end
      else if ThreadEntry.State = THREAD_STATE_WAIT_TIMEOUT then
       begin
        {Remove from Queue}
        Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
        if Result <> ERROR_SUCCESS then Exit;

        {Set WaitResult}
        ThreadEntry.WaitResult:=ERROR_SUCCESS;

        {Set WaitList}
        ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
       end
      else
       begin
        Exit;
       end;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_READY;

      {Do not update Kernel Time}

      {Check CPU}
      if SCHEDULER_CPU_COUNT > 1 then
       begin
        if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectCPU[CPUGetCurrent]);
          {$ENDIF}

          {Update CPU}
          ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
         end
        else
         begin
          {Check Affinity}
          if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
           begin
            {$IFDEF SCHEDULER_DEBUG}
            Inc(SchedulerSelectAffinity[CPUGetCurrent]);
            {$ENDIF}

            {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
            for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
             begin
              if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
               begin
                {Update CPU}
                ThreadEntry.CurrentCPU:=Count;
                ThreadEntry.TargetCPU:=Count;

                Break;
               end;
             end;
           end;
         end;
       end;

      {Check Priority}
      if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectPriority[CPUGetCurrent]);
        {$ENDIF}

        {Update Priority}
        ThreadEntry.Priority:=ThreadEntry.TargetPriority;
       end;

      {Enqueue Thread (Current CPU and Priority)}
      Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
     finally
      {Release the Lock}
      if SCHEDULER_FIQ_ENABLED then
       begin
        SpinUnlockIRQFIQ(ThreadEntry.Lock);
       end
      else
       begin
        SpinUnlockIRQ(ThreadEntry.Lock);
       end;
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin
   Result:=ERROR_NOT_FOUND;
  end;
end;

{==============================================================================}

function ThreadAbandon(List:TListHandle):LongWord;
{Release the first thread waiting on the supplied list and return with WAIT_ABANDONED}
{List: Handle of List entry to release thread from}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Caller must hold the lock on the synchronisation object containing the list}
var
 Count:LongWord;
 ResultCode:LongWord;
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
 ListElement:PListElement;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Abandon (List=' + HandleToHex(List) + ')');
 {$ENDIF}

 {Check List}
 if List = INVALID_HANDLE_VALUE then Exit;

 {Get List First}
 ListElement:=ListGetFirstEx(List,True);
 if ListElement <> nil then
  begin
   {Get Thread}
   Thread:=ListElement.Thread;
   if Thread = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   ThreadEntry:=PThreadEntry(Thread);
   if ThreadEntry = nil then Exit;
   if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

   {Acquire the Lock}
   if SCHEDULER_FIQ_ENABLED then
    begin
     ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
    end
   else
    begin
     ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
    end;
   if ResultCode = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

      Result:=ERROR_OPERATION_FAILED;

      {Check State}
      if ThreadEntry.State = THREAD_STATE_TERMINATED then
       begin
        Exit;
       end
      else if ThreadEntry.State = THREAD_STATE_RUNNING then
       begin
        Exit;
       end;

      {Check State}
      if ThreadEntry.State = THREAD_STATE_WAIT then
       begin
        {Set WaitResult}
        ThreadEntry.WaitResult:=WAIT_ABANDONED;

        {Set WaitList}
        ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
       end
      else if ThreadEntry.State = THREAD_STATE_WAIT_TIMEOUT then
       begin
        {Remove from Queue}
        Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
        if Result <> ERROR_SUCCESS then Exit;

        {Set WaitResult}
        ThreadEntry.WaitResult:=WAIT_ABANDONED;

        {Set WaitList}
        ThreadEntry.WaitList:=INVALID_HANDLE_VALUE;
       end
      else
       begin
        Exit;
       end;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_READY;

      {Do not update Kernel Time}

      {Check CPU}
      if SCHEDULER_CPU_COUNT > 1 then
       begin
        if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectCPU[CPUGetCurrent]);
          {$ENDIF}

          {Update CPU}
          ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
         end
        else
         begin
          {Check Affinity}
          if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
           begin
            {$IFDEF SCHEDULER_DEBUG}
            Inc(SchedulerSelectAffinity[CPUGetCurrent]);
            {$ENDIF}

            {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
            for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
             begin
              if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
               begin
                {Update CPU}
                ThreadEntry.CurrentCPU:=Count;
                ThreadEntry.TargetCPU:=Count;

                Break;
               end;
             end;
           end;
         end;
       end;

      {Check Priority}
      if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectPriority[CPUGetCurrent]);
        {$ENDIF}

        {Update Priority}
        ThreadEntry.Priority:=ThreadEntry.TargetPriority;
       end;

      {Enqueue Thread (Current CPU and Priority)}
      Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
     finally
      {Release the Lock}
      if SCHEDULER_FIQ_ENABLED then
       begin
        SpinUnlockIRQFIQ(ThreadEntry.Lock);
       end
      else
       begin
        SpinUnlockIRQ(ThreadEntry.Lock);
       end;
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin
   Result:=ERROR_NOT_FOUND;
  end;
end;

{==============================================================================}

function ThreadWaitTerminate(Thread:TThreadHandle;Timeout:LongWord):LongWord;
{Make the current thread wait until the specified thread has terminated}
{Thread: Handle of the thread to wait on}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The wait can be abandoned by calling ThreadWake with the handle of the waiting thread}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Wait Terminate (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   Unlock:=True;
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    {Check Timeout}
    if Timeout = 0 then
     begin
      {Check State}
      Result:=ERROR_WAIT_TIMEOUT;
      if ThreadEntry.State <> THREAD_STATE_TERMINATED then Exit;
     end;

    {Check State}
    if ThreadEntry.State <> THREAD_STATE_TERMINATED then
     begin
      {Check List}
      if ThreadEntry.List = INVALID_HANDLE_VALUE then
       begin
        {Create List} {Note: List is always preallocated on threads to prevent IRQ/FIQ deadlock}
        ThreadEntry.List:=ListCreateEx(LIST_TYPE_WAIT_THREAD,SchedulerGetListFlags(LIST_TYPE_WAIT_THREAD));
       end;

      {Check Timeout}
      if Timeout = INFINITE then
       begin
        {Wait on Thread}
        if SCHEDULER_FIQ_ENABLED then
         begin
          ThreadWait(ThreadEntry.List,ThreadEntry.Lock,LOCK_FLAG_IRQFIQ);
         end
        else
         begin
          ThreadWait(ThreadEntry.List,ThreadEntry.Lock,LOCK_FLAG_IRQ);
         end;
        Unlock:=False;

        {Check Result}
        WaitResult:=ThreadGetWaitResult;
        if WaitResult = WAIT_TIMEOUT then
         begin
          Result:=ERROR_WAIT_TIMEOUT;
          Exit;
         end
        else if WaitResult = WAIT_ABANDONED then
         begin
          Result:=ERROR_WAIT_ABANDONED;
          Exit;
         end
        else if WaitResult <> ERROR_SUCCESS then
         begin
          Result:=WaitResult;
          Exit;
         end;
       end
      else
       begin
        {Wait on Thread with Timeout}
        if SCHEDULER_FIQ_ENABLED then
         begin
          ThreadWaitEx(ThreadEntry.List,ThreadEntry.Lock,LOCK_FLAG_IRQFIQ,Timeout);
         end
        else
         begin
          ThreadWaitEx(ThreadEntry.List,ThreadEntry.Lock,LOCK_FLAG_IRQ,Timeout);
         end;
        Unlock:=False;

        {Check Result}
        WaitResult:=ThreadGetWaitResult;
        if WaitResult = WAIT_TIMEOUT then
         begin
          Result:=ERROR_WAIT_TIMEOUT;
          Exit;
         end
        else if WaitResult = WAIT_ABANDONED then
         begin
          Result:=ERROR_WAIT_ABANDONED;
          Exit;
         end
        else if WaitResult <> ERROR_SUCCESS then
         begin
          Result:=WaitResult;
          Exit;
         end;
       end;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if Unlock then
     begin
      if SCHEDULER_FIQ_ENABLED then
       begin
        SpinUnlockIRQFIQ(ThreadEntry.Lock);
       end
      else
       begin
        SpinUnlockIRQ(ThreadEntry.Lock);
       end;
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadSuspend(Thread:TThreadHandle):LongWord;
{Suspend a thread, placing it in hibernation}
{Thread: Handle of thread to suspend}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Suspending a thread from another thread is not safe unless specific precautions are taken
       to prevent deadlocks
       It is normally safe for a thread to suspend itself as long as it releases any locks it is
       holding that may be required by other threads before suspending}
var
 Reschedule:Boolean;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Suspend (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check the Handle}
 if Thread = IRQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = FIQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = SWI_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = IDLE_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   Reschedule:=False;
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if (ThreadEntry.State <> THREAD_STATE_RUNNING) and (ThreadEntry.State <> THREAD_STATE_READY) then
     begin
      Exit;
     end;

    {Check Ready}
    if ThreadEntry.State = THREAD_STATE_READY then
     begin
      {Remove from queue}
      QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);

      {Set State}
      ThreadEntry.State:=THREAD_STATE_SUSPENDED;

      {Do not update Kernel Time}
     end
    else
     begin
      {Set State}
      ThreadEntry.State:=THREAD_STATE_SUSPENDED;

      {$IFDEF THREAD_STATISTICS}
      {Check Current Thread (It is normally considered unsafe to suspend another thread that is running)}
      if Thread = ThreadGetCurrent then
       begin
        {Update Kernel Time}
        if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
         begin
          Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
         end;
       end;
      {$ENDIF}

      {Reschedule}
      Reschedule:=True;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;

   {Check Reschedule}
   if Reschedule then SchedulerReschedule(False);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadResume(Thread:TThreadHandle):LongWord;
{Resume a suspended thread, making it ready}
{Thread: Handle of thread to resume}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Resume (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Check the Handle}
 if Thread = IRQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = FIQ_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = SWI_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;
 if Thread = IDLE_THREAD_HANDLE[ThreadEntry.CurrentCPU] then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State <> THREAD_STATE_SUSPENDED then
     begin
      Exit;
     end;

    {Set State}
    ThreadEntry.State:=THREAD_STATE_READY;

    {Do not update Kernel Time}

    {Check CPU}
    if SCHEDULER_CPU_COUNT > 1 then
     begin
      if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectCPU[CPUGetCurrent]);
        {$ENDIF}

        {Update CPU}
        ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
       end
      else
       begin
        {Check Affinity}
        if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectAffinity[CPUGetCurrent]);
          {$ENDIF}

          {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
          for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
           begin
            if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
             begin
              {Update CPU}
              ThreadEntry.CurrentCPU:=Count;
              ThreadEntry.TargetCPU:=Count;

              Break;
             end;
           end;
         end;
       end;
     end;

    {Check Priority}
    if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
     begin
      {$IFDEF SCHEDULER_DEBUG}
      Inc(SchedulerSelectPriority[CPUGetCurrent]);
      {$ENDIF}

      {Update Priority}
      ThreadEntry.Priority:=ThreadEntry.TargetPriority;
     end;

    {Enqueue Thread (Current CPU and Priority)}
    Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadWaitMessage:LongWord;
{Make the current thread wait until a message is received (indefinitely)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The received message is not removed from the message list}
var
 Message:TMessage;
begin
 {}
 Result:=ThreadReceiveMessageEx(Message,INFINITE,False);
end;

{==============================================================================}

function ThreadSendMessage(Thread:TThreadHandle;const Message:TMessage):LongWord;
{Send a message to another thread}
{Thread: Handle of thread to send to}
{Message: Contents of message to send}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Send Message (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end;

    {Check Count}
    if ThreadEntry.Messages.Count >= ThreadEntry.Messages.Maximum then
     begin
      Result:=ERROR_NO_MORE_ITEMS;
      Exit;
     end;

    {Write Message}
    PMessage(PtrUInt(ThreadEntry.Messages.List) + PtrUInt(((ThreadEntry.Messages.Start + ThreadEntry.Messages.Count) mod ThreadEntry.Messages.Maximum) * SizeOf(TMessage)))^:=Message;

    {Update Count}
    Inc(ThreadEntry.Messages.Count);

    {Check State}
    if ThreadEntry.State = THREAD_STATE_RECEIVE then
     begin
      {Set ReceiveResult}
      ThreadEntry.ReceiveResult:=ERROR_SUCCESS;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_READY;

      {Do not update Kernel Time}

      {Check CPU}
      if SCHEDULER_CPU_COUNT > 1 then
       begin
        if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectCPU[CPUGetCurrent]);
          {$ENDIF}

          {Update CPU}
          ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
         end
        else
         begin
          {Check Affinity}
          if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
           begin
            {$IFDEF SCHEDULER_DEBUG}
            Inc(SchedulerSelectAffinity[CPUGetCurrent]);
            {$ENDIF}

            {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
            for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
             begin
              if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
               begin
                {Update CPU}
                ThreadEntry.CurrentCPU:=Count;
                ThreadEntry.TargetCPU:=Count;

                Break;
               end;
             end;
           end;
         end;
       end;

      {Check Priority}
      if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectPriority[CPUGetCurrent]);
        {$ENDIF}

        {Update Priority}
        ThreadEntry.Priority:=ThreadEntry.TargetPriority;
       end;

      {Enqueue Thread (Current CPU and Priority)}
      Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else if ThreadEntry.State = THREAD_STATE_RECEIVE_TIMEOUT then
     begin
      {Remove from Queue}
      Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
      if Result <> ERROR_SUCCESS then Exit;

      {Set ReceiveResult}
      ThreadEntry.ReceiveResult:=ERROR_SUCCESS;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_READY;

      {Do not update Kernel Time}

      {Check CPU}
      if SCHEDULER_CPU_COUNT > 1 then
       begin
        if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectCPU[CPUGetCurrent]);
          {$ENDIF}

          {Update CPU}
          ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
         end
        else
         begin
          {Check Affinity}
          if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
           begin
            {$IFDEF SCHEDULER_DEBUG}
            Inc(SchedulerSelectAffinity[CPUGetCurrent]);
            {$ENDIF}

            {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
            for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
             begin
              if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
               begin
                {Update CPU}
                ThreadEntry.CurrentCPU:=Count;
                ThreadEntry.TargetCPU:=Count;

                Break;
               end;
             end;
           end;
         end;
       end;

      {Check Priority}
      if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectPriority[CPUGetCurrent]);
        {$ENDIF}

        {Update Priority}
        ThreadEntry.Priority:=ThreadEntry.TargetPriority;
       end;

      {Enqueue Thread (Current CPU and Priority)}
      Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadReceiveMessage(var Message:TMessage):LongWord;
{Make the current thread wait to receive a message (indefinitely)}
{Message: The received message if successful, undefined on error}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ThreadReceiveMessageEx(Message,INFINITE,True);
end;

{==============================================================================}

function ThreadReceiveMessageEx(var Message:TMessage;Timeout:LongWord;Remove:Boolean):LongWord;
{Make the current thread wait to receive a message}
{Message: The received message if successful, undefined on error}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Remove: If true then remove the received message from the message list}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Ticks:Integer;
 Reschedule:Boolean;
 ResultCode:LongWord;
 ReceiveResult:LongWord;
 Thread:TThreadHandle;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Receive Message (Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}

 {Get Thread}
 Thread:=ThreadGetCurrent;
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   Reschedule:=False;
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end;

    {Check Timeout}
    if Timeout = 0 then
     begin
      {Check Count}
      Result:=ERROR_WAIT_TIMEOUT;
      if ThreadEntry.Messages.Count = 0 then Exit;
     end;

    {Check for Message}
    if ThreadEntry.Messages.Count > 0 then
     begin
      {Read Message}
      Message:=PMessage(PtrUInt(ThreadEntry.Messages.List) + PtrUInt(ThreadEntry.Messages.Start * SizeOf(TMessage)))^;

      {Check Remove}
      if Remove then
       begin
        {Update Start}
        ThreadEntry.Messages.Start:=(ThreadEntry.Messages.Start + 1) mod ThreadEntry.Messages.Maximum;

        {Update Count}
        Dec(ThreadEntry.Messages.Count);
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     end
    else
     begin
      {Check Timeout}
      if Timeout = INFINITE then
       begin
        {Wait on Message}
        {Set State}
        ThreadEntry.State:=THREAD_STATE_RECEIVE;

        {$IFDEF THREAD_STATISTICS}
        {Update Kernel Time}
        if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
         begin
          Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
         end;
        {$ENDIF}

        {Reschedule}
        Reschedule:=True;
       end
      else
       begin
        {Wait on Message with Timeout}
        {Calculate Ticks}
        Ticks:=SCHEDULER_INTERRUPTS_PER_MILLISECOND * Timeout;

        {Set State}
        ThreadEntry.State:=THREAD_STATE_RECEIVE_TIMEOUT;

        {$IFDEF THREAD_STATISTICS}
        {Update Kernel Time}
        if SchedulerThreadQuantum[ThreadEntry.CurrentCPU] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
         begin
          Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[ThreadEntry.CurrentCPU]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
         end;
        {$ENDIF}

        {Insert in Queue (Current CPU)}
        if QueueInsertKey(SchedulerGetQueueHandle(ThreadEntry.CurrentCPU,QUEUE_TYPE_SCHEDULE_TIMEOUT),Thread,Ticks) <> ERROR_SUCCESS then Exit;

        {Reschedule}
        Reschedule:=True;
       end;
     end;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;

   {Check Reschedule}
   if Reschedule then
    begin
     {Reschedule}
     SchedulerReschedule(False);

     {Check Result}
     ReceiveResult:=ThreadGetReceiveResult;
     if ReceiveResult = WAIT_TIMEOUT then
      begin
       Result:=ERROR_WAIT_TIMEOUT;
       Exit;
      end
     else if ReceiveResult = WAIT_ABANDONED then
      begin
       Result:=ERROR_WAIT_ABANDONED;
       Exit;
      end
     else if ReceiveResult <> ERROR_SUCCESS then
      begin
       Result:=ReceiveResult;
       Exit;
      end;

     {Receive Message (No Wait)}
     Result:=ThreadReceiveMessageEx(Message,0,Remove);
    end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadAbandonMessage(Thread:TThreadHandle):LongWord;
{Tell another thread to abandon waiting for a message}
{Thread: Handle of thread to abandon waiting}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The waiting thread will return with ERROR_WAIT_ABANDONED or ERROR_WAIT_TIMEOUT}
var
 Count:LongWord;
 ResultCode:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Abandon Message (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

    Result:=ERROR_OPERATION_FAILED;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_TERMINATED then
     begin
      Exit;
     end
    else if ThreadEntry.State = THREAD_STATE_RUNNING then
     begin
      Exit;
     end;

    {Check State}
    if ThreadEntry.State = THREAD_STATE_RECEIVE then
     begin
      {Set ReceiveResult}
      ThreadEntry.ReceiveResult:=WAIT_ABANDONED;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_READY;

      {Do not update Kernel Time}

      {Check CPU}
      if SCHEDULER_CPU_COUNT > 1 then
       begin
        if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectCPU[CPUGetCurrent]);
          {$ENDIF}

          {Update CPU}
          ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
         end
        else
         begin
          {Check Affinity}
          if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
           begin
            {$IFDEF SCHEDULER_DEBUG}
            Inc(SchedulerSelectAffinity[CPUGetCurrent]);
            {$ENDIF}

            {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
            for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
             begin
              if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
               begin
                {Update CPU}
                ThreadEntry.CurrentCPU:=Count;
                ThreadEntry.TargetCPU:=Count;

                Break;
               end;
             end;
           end;
         end;
       end;

      {Check Priority}
      if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectPriority[CPUGetCurrent]);
        {$ENDIF}

        {Update Priority}
        ThreadEntry.Priority:=ThreadEntry.TargetPriority;
       end;

      {Enqueue Thread (Current CPU and Priority)}
      Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else if ThreadEntry.State = THREAD_STATE_RECEIVE_TIMEOUT then
     begin
      {Remove from Queue}
      Result:=QueueDeleteKey(ThreadEntry.ScheduleQueue,Thread);
      if Result <> ERROR_SUCCESS then Exit;

      {Set ReceiveResult}
      ThreadEntry.ReceiveResult:=WAIT_TIMEOUT;

      {Set State}
      ThreadEntry.State:=THREAD_STATE_READY;

      {Do not update Kernel Time}

      {Check CPU}
      if SCHEDULER_CPU_COUNT > 1 then
       begin
        if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
         begin
          {$IFDEF SCHEDULER_DEBUG}
          Inc(SchedulerSelectCPU[CPUGetCurrent]);
          {$ENDIF}

          {Update CPU}
          ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
         end
        else
         begin
          {Check Affinity}
          if (ThreadEntry.Affinity and (1 shl CPUGetCurrent)) = 0 then
           begin
            {$IFDEF SCHEDULER_DEBUG}
            Inc(SchedulerSelectAffinity[CPUGetCurrent]);
            {$ENDIF}

            {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
            for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
             begin
              if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
               begin
                {Update CPU}
                ThreadEntry.CurrentCPU:=Count;
                ThreadEntry.TargetCPU:=Count;

                Break;
               end;
             end;
           end;
         end;
       end;

      {Check Priority}
      if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
       begin
        {$IFDEF SCHEDULER_DEBUG}
        Inc(SchedulerSelectPriority[CPUGetCurrent]);
        {$ENDIF}

        {Update Priority}
        ThreadEntry.Priority:=ThreadEntry.TargetPriority;
       end;

      {Enqueue Thread (Current CPU and Priority)}
      Result:=QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    if SCHEDULER_FIQ_ENABLED then
     begin
      SpinUnlockIRQFIQ(ThreadEntry.Lock);
     end
    else
     begin
      SpinUnlockIRQ(ThreadEntry.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ThreadLock(Thread:TThreadHandle):LongWord;
{Lock a thread allowing access to internal structures such as the thread stack}
{Thread: Handle of thread to lock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Locking a thread will also disable IRQ or FIQ depending on scheduler
       settings. The lock should only be held for the briefest time possible}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Lock (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Acquire the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   Result:=SpinLockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   Result:=SpinLockIRQ(ThreadEntry.Lock);
  end;
end;

{==============================================================================}

function ThreadUnlock(Thread:TThreadHandle):LongWord;
{Unlok a thread that was locked by ThreadLock}
{Thread: Handle of thread to unlock}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Locking a thread will also disable IRQ or FIQ depending on scheduler
       settings. The lock should only be held for the briefest time possible}
var
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Thread Unlock (Handle=' + HandleToHex(Thread) + ')');
 {$ENDIF}

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 ThreadEntry:=PThreadEntry(Thread);
 if ThreadEntry = nil then Exit;
 if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

 {Release the Lock}
 if SCHEDULER_FIQ_ENABLED then
  begin
   Result:=SpinUnlockIRQFIQ(ThreadEntry.Lock);
  end
 else
  begin
   Result:=SpinUnlockIRQ(ThreadEntry.Lock);
  end;
end;

{==============================================================================}

procedure ThreadTimer(Data:Pointer);
{Procedure called internally to process terminated threads}

{Note: Not intended to be called directly by applications}
var
 CurrentCPU:LongWord;
begin
 {}
 {Get Current CPU}
 CurrentCPU:=CPUGetCurrent;

 {Check Terminated}
 while QueueFirstKeyEx(SchedulerTerminationQueue[CurrentCPU],True,False) <= 0 do
  begin
   {$IFDEF SCHEDULER_DEBUG}
   Inc(SchedulerTerminationCounter[CurrentCPU]);
   {$ENDIF}

   {Destroy Thread}
   ThreadDestroy(QueueDequeueEx(SchedulerTerminationQueue[CurrentCPU],False,True));
  end;
 {Unlock Queue}
 QueueUnlock(SchedulerTerminationQueue[CurrentCPU]);
end;

{==============================================================================}
{==============================================================================}
{Scheduler Functions}
function SchedulerCheck(CPUID:LongWord):LongWord;
{Check if the sleep queue is empty, if not then decrement the first key
 Then check if the timeout queue is empty, if not then decrement the first key

 If either key reaches zero, return success to indicate there are threads to be
 woken or threads whose timeout has expired

 Finally check if the termination queue is empty, if not then decrement the first
 key

 Items will be removed from the termination queue by SchedulerReschedule}
{CPUID: The ID of the current CPU}
{Return: ERROR_SUCCESS if either first key is zero, ERROR_NO_MORE_ITEMS if both queues are empty or another error code on failure}
{Note: Called by scheduler interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Thread}
 Result:=ERROR_FUNCTION_FAILED;
 if SCHEDULER_FIQ_ENABLED then
  begin
   if ThreadGetCurrent <> FIQ_THREAD_HANDLE[CPUID] then Exit;
  end
 else
  begin
   if ThreadGetCurrent <> IRQ_THREAD_HANDLE[CPUID] then Exit;
  end;

 {Check the Handler}
 if Assigned(SchedulerCheckHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerCheckHandler(CPUID);
  end
 else
  begin
   {Use the Default method}
   {Setup Result}
   Result:=ERROR_NO_MORE_ITEMS;

   {Check Sleep Queue}
   if QueueDecrementKey(SchedulerSleepQueue[CPUID]) <= 0 then
    begin
     Result:=ERROR_SUCCESS;
    end;

   {Check Timeout Queue}
   if QueueDecrementKey(SchedulerTimeoutQueue[CPUID]) <= 0 then
    begin
     Result:=ERROR_SUCCESS;
    end;

   {Check Termination Queue}
   QueueDecrementKey(SchedulerTerminationQueue[CPUID]);
  end;
end;

{==============================================================================}

function SchedulerWakeup(CPUID:LongWord):LongWord;
{Remove all threads from the sleep queue that have no more time to sleep

 Threads will be placed back on the ready queue for rescheduling}
{CPUID: The ID of the current CPU}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Called by scheduler interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Thread}
 Result:=ERROR_FUNCTION_FAILED;
 if SCHEDULER_FIQ_ENABLED then
  begin
   if ThreadGetCurrent <> FIQ_THREAD_HANDLE[CPUID] then Exit;
  end
 else
  begin
   if ThreadGetCurrent <> IRQ_THREAD_HANDLE[CPUID] then Exit;
  end;

 {Check the Handler}
 if Assigned(SchedulerWakeupHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerWakeupHandler(CPUID);
  end
 else
  begin
   {Use the Default method}
   {Ready Threads}
   while QueueFirstKeyEx(SchedulerSleepQueue[CPUID],True,False) <= 0 do
    begin
     ThreadReady(QueueDequeueEx(SchedulerSleepQueue[CPUID],False,True),False);
    end;
   {Unlock Queue}
   QueueUnlock(SchedulerSleepQueue[CPUID]);

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SchedulerExpire(CPUID:LongWord):LongWord;
{Remove all threads from the timeout queue that have no more time to wait

 Threads will be placed back on the ready queue for rescheduling but will
 return with an error indicating the timeout expired}
{CPUID: The ID of the current CPU}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Called by scheduler interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Thread}
 Result:=ERROR_FUNCTION_FAILED;
 if SCHEDULER_FIQ_ENABLED then
  begin
   if ThreadGetCurrent <> FIQ_THREAD_HANDLE[CPUID] then Exit;
  end
 else
  begin
   if ThreadGetCurrent <> IRQ_THREAD_HANDLE[CPUID] then Exit;
  end;

 {Check the Handler}
 if Assigned(SchedulerExpireHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerExpireHandler(CPUID);
  end
 else
  begin
   {Use the Default method}
   {Timeout Threads}
   while QueueFirstKeyEx(SchedulerTimeoutQueue[CPUID],True,False) <= 0 do
    begin
     ThreadTimeout(QueueDequeueEx(SchedulerTimeoutQueue[CPUID],False,True));
    end;
   {Unlock Queue}
   QueueUnlock(SchedulerTimeoutQueue[CPUID]);

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SchedulerSwitch(CPUID:LongWord;Thread:TThreadHandle):TThreadHandle;
{Perform a preemptive thread switch operation under an interrupt handler

 The next thread to run will be selected based on remaining quantum of the current
 thread, ready threads at higher priority levels and scheduler priority quantum for
 fair scheduling of lower priority threads}
{CPUID: The ID of the current CPU}
{Thread: The handle of the currently running thread (Before IRQ or FIQ occurred)}
{Return: The handle of the current thread which may be the old thread or a new thread}
{Note: Called by scheduler interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
var
 ResultCode:LongWord;

 NewThread:TThreadHandle;

 ThreadEntry:PThreadEntry;
 NewThreadEntry:PThreadEntry;
begin
 {}
 Result:=Thread;

 {$IFDEF SCHEDULER_DEBUG}
 Inc(SchedulerSwitchEntry[CPUID]);
 {$ENDIF}

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Thread}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if ThreadGetCurrent <> FIQ_THREAD_HANDLE[CPUID] then Exit;
  end
 else
  begin
   if ThreadGetCurrent <> IRQ_THREAD_HANDLE[CPUID] then Exit;
  end;

 {Check the Handler}
 if Assigned(SchedulerSwitchHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerSwitchHandler(CPUID,Thread);
  end
 else
  begin
   {Use the Default method}
   {Check Thread}
   if Thread = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   ThreadEntry:=PThreadEntry(Thread);
   if ThreadEntry = nil then Exit;
   if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

   {Acquire the Lock}
   if SCHEDULER_FIQ_ENABLED then
    begin
     ResultCode:=SpinLockIRQFIQ(ThreadEntry.Lock);
    end
   else
    begin
     ResultCode:=SpinLockIRQ(ThreadEntry.Lock);
    end;
   if ResultCode = ERROR_SUCCESS then
    begin
     try
      {Check Thread Quantum}
      if SchedulerThreadQuantum[CPUID] > 0 then
       begin
        {Decrement Thread Quantum}
        Dec(SchedulerThreadQuantum[CPUID]);
       end;

      {Select New Thread}
      NewThread:=SchedulerSelect(CPUID,Thread,False);
      if NewThread <> INVALID_HANDLE_VALUE then
       begin
        {Check the Handle}
        NewThreadEntry:=PThreadEntry(NewThread);
        if NewThreadEntry = nil then Exit;
        if NewThreadEntry.Signature <> THREAD_SIGNATURE then Exit;
       end;

      {$IFDEF SCHEDULER_DEBUG}
      SchedulerSwitchThread[CPUID]:=NewThread;
      {$ENDIF}

      {Check New Thread}
      if (NewThread = Thread) or (NewThread = INVALID_HANDLE_VALUE) then
       begin
        {Scheduler Select handles state if no selection or same selection}
        {$IFDEF SCHEDULER_DEBUG}
        if NewThread = INVALID_HANDLE_VALUE then
         begin
          Inc(SchedulerSwitchInvalid[CPUID]);
         end
        else
         begin
          Inc(SchedulerSwitchCurrent[CPUID]);
         end;
        {$ENDIF}
       end
      else
       begin
        {Acquire the Lock}
        if SCHEDULER_FIQ_ENABLED then
         begin
          ResultCode:=SpinLockIRQFIQ(NewThreadEntry.Lock);
         end
        else
         begin
          ResultCode:=SpinLockIRQ(NewThreadEntry.Lock);
         end;
        if ResultCode = ERROR_SUCCESS then
         begin
          try
           {$IFDEF SCHEDULER_DEBUG}
           Inc(SchedulerSwitchCounter[CPUID]);
           {$ENDIF}

           {Set State}
           NewThreadEntry.State:=THREAD_STATE_RUNNING;

           {Set CPU}
           NewThreadEntry.CurrentCPU:=CPUID;

           {$IFDEF THREAD_STATISTICS}
           {Update Switch Count}
           Inc(NewThreadEntry.SwitchCount);
           {$ENDIF}

           if SCHEDULER_FIQ_ENABLED then
            begin
             {Perform a Context Switch (FIQ)}
             ContextSwitchFIQ(@ThreadEntry.StackPointer,@NewThreadEntry.StackPointer,NewThread);
            end
           else
            begin
             {Perform a Context Switch (IRQ)}
             ContextSwitchIRQ(@ThreadEntry.StackPointer,@NewThreadEntry.StackPointer,NewThread);
            end;

           {Return Result}
           Result:=NewThread;
          finally
           {Release the Lock}
           if SCHEDULER_FIQ_ENABLED then
            begin
             SpinUnlockIRQFIQ(NewThreadEntry.Lock);
            end
           else
            begin
             SpinUnlockIRQ(NewThreadEntry.Lock);
            end;
          end;
         end
        else
         begin
          {Nothing}
         end;
       end;
     finally
      {Release the Lock}
      if SCHEDULER_FIQ_ENABLED then
       begin
        SpinUnlockIRQFIQ(ThreadEntry.Lock);
       end
      else
       begin
        SpinUnlockIRQ(ThreadEntry.Lock);
       end;
     end;
    end
   else
    begin
     {Nothing}
    end;
  end;
end;

{==============================================================================}

function SchedulerSelect(CPUID:LongWord;Thread:TThreadHandle;Yield:Boolean):TThreadHandle;
{Select the next thread to be run based on state, yield, quantum and priority}
{CPUID: The ID of the current CPU}
{Thread: The handle of the currently running thread (Before IRQ or FIQ occurred or when Reschedule was called)}
{Yield: True if the current thread is giving up its remaining time slice}
{Return: The handle of the next thread to run or INVALID_HANDLE_VALUE on no selection or error}
{Note: Called either by scheduler interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread
       Or by scheduler reschedule with IRQ or FIQ disabled and running on the current thread}
{Note: Caller must either hold a lock on the current thread or have disabled IRQ or FIQ}
var
 Count:LongWord;
 Value:LongWord;
 MinCPU:LongWord;
 MaxCPU:LongWord;
 MinCount:LongWord;
 MaxCount:LongWord;
 Priority:LongWord;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {$IFDEF SCHEDULER_DEBUG}
 Inc(SchedulerSelectEntry[CPUID]);
 {$ENDIF}

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check the Handler}
 if Assigned(SchedulerSelectHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerSelectHandler(CPUID,Thread,Yield);
  end
 else
  begin
   {Use the Default method}
   {Check Thread}
   if Thread = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   ThreadEntry:=PThreadEntry(Thread);
   if ThreadEntry = nil then Exit;
   if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

   {Check State}
   if ThreadEntry.State = THREAD_STATE_RUNNING then
    begin
     {Check Initialization Completed}
     if not(InitializationCompleted[CPUID]) then
      begin
       {Return Thread}
       Result:=Thread;
       Exit;
      end;

     {Check Yield, Thread Quantum and Higher Priority Threads}
     {$IFNDEF SCHEDULER_YIELD_ALTERNATE}
     if not(Yield) and (SchedulerThreadQuantum[CPUID] > 0) and (ThreadEntry.Priority >= FirstBitSet(SchedulerPriorityMask[CPUID])) then
     {$ELSE}
     if Yield and (ThreadEntry.Priority > FirstBitSet(SchedulerPriorityMask[CPUID])) then
      begin
       {Return Thread}
       Result:=Thread;
       Exit;
      end
     else if (SchedulerThreadQuantum[CPUID] > 0) and (ThreadEntry.Priority >= FirstBitSet(SchedulerPriorityMask[CPUID])) then
     {$ENDIF}
      begin
       {Return Thread}
       Result:=Thread;
       Exit;
      end;

     {Set State}
     ThreadEntry.State:=THREAD_STATE_READY;

     {$IFDEF THREAD_STATISTICS}
     {Update Kernel Time}
     if SchedulerThreadQuantum[CPUID] <= (SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority]) then
      begin
       Inc(ThreadEntry.KernelTime,((SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority] - SchedulerThreadQuantum[CPUID]) * TIME_TICKS_PER_SCHEDULER_INTERRUPT));
      end;
     {$ENDIF}

     {Check CPU}
     if SCHEDULER_CPU_COUNT > 1 then
      begin
       if ThreadEntry.CurrentCPU <> ThreadEntry.TargetCPU then
        begin
         {$IFDEF SCHEDULER_DEBUG}
         Inc(SchedulerSelectCPU[CPUID]);
         {$ENDIF}

         {Update CPU}
         ThreadEntry.CurrentCPU:=ThreadEntry.TargetCPU;
        end
       else
        begin
         {Check Affinity}
         if (ThreadEntry.Affinity and (1 shl CPUID)) = 0 then
          begin
           {$IFDEF SCHEDULER_DEBUG}
           Inc(SchedulerSelectAffinity[CPUID]);
           {$ENDIF}

           {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
           for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
            begin
             if (ThreadEntry.Affinity and (1 shl Count)) <> 0 then
              begin
               {Update CPU}
               ThreadEntry.CurrentCPU:=Count;
               ThreadEntry.TargetCPU:=Count;

               Break;
              end;
            end;
          end;
        end;
      end;

     {Check Priority}
     if ThreadEntry.Priority <> ThreadEntry.TargetPriority then
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerSelectPriority[CPUID]);
       {$ENDIF}

       {Update Priority}
       ThreadEntry.Priority:=ThreadEntry.TargetPriority;
      end;

     {Enqueue Thread (Current CPU and Priority)}
     if QueueEnqueue(SchedulerGetQueueHandleEx(ThreadEntry.CurrentCPU,ThreadEntry.Priority),Thread) <> ERROR_SUCCESS then
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerSelectFailure[CPUID]);
       {$ENDIF}

       {Restore State}
       ThreadEntry.State:=THREAD_STATE_RUNNING;

       {Return Thread}
       Result:=Thread;
       Exit;
      end;
    end;

   {Check Migration (Boot CPU Only)}
   if (SCHEDULER_CPU_COUNT > 1) and (CPUID = SCHEDULER_CPU_BOOT) and (SchedulerThreadMigration = SCHEDULER_MIGRATION_ENABLED) then
    begin
     {Check Migration Quantum}
     if SchedulerMigrationQuantum > 0 then
      begin
       {Decrement Migration Quantum}
       Dec(SchedulerMigrationQuantum);
      end;

     {Check Migration Quantum}
     if SchedulerMigrationQuantum < 1 then
      begin
       {Setup Min/Max}
       MinCPU:=CPU_ID_0;
       MaxCPU:=CPU_ID_0;
       MinCount:=$FFFFFFFF;
       MaxCount:=0;

       {Check CPUs 0 to SCHEDULER_CPU_COUNT - 1}
       for Count:=0 to SCHEDULER_CPU_COUNT - 1 do
        begin
         {Check Thread Allocation}
         if SchedulerThreadAllocation[Count] = SCHEDULER_ALLOCATION_ENABLED then
          begin
           {Get Thread Count}
           Value:=SchedulerThreadCount[Count];

           {Check Min Count}
           if Value < MinCount then
            begin
             MinCPU:=Count;
             MinCount:=Value;
            end;

           {Check Max Count}
           if Value > MaxCount then
            begin
             MaxCPU:=Count;
             MaxCount:=Value;
            end;
          end;
        end;

       {Compare Min/Max}
       if MinCPU <> MaxCPU then
        begin
         if (MaxCount - MinCount) > (MinCount + SCHEDULER_MIGRATION_OFFSET) then
          begin
           {Try to migrate a thread from MaxCPU to MinCPU}
           //To Do //Compare affinity here (QueueFind), no need in normal select due to Queue per CPU
                   //Perhaps this should just set TargetCPU and allow rescheduling to migrate the thread ?
                   //Yes, Just do QueueFind and set TargetCPU for next Reschedule

           {$IFDEF SCHEDULER_DEBUG}
           Inc(SchedulerMigrationCounter);
           {$ENDIF}
          end;
        end;

       {Set Migration Quantum}
       SchedulerMigrationQuantum:=SCHEDULER_MIGRATION_QUANTUM;
      end;
    end;

   {Check Starvation Quantum}
   if SchedulerStarvationQuantum[CPUID] < 1 then
    begin
     {$IFDEF SCHEDULER_DEBUG}
     Inc(SchedulerSelectForce[CPUID]);
     {$ENDIF}

     {Get Next Priority}
     Priority:=SchedulerStarvationNext[CPUID];

     {Check Priority}
     if Priority >= THREAD_PRIORITY_MINIMUM then Dec(Priority);
     if Priority < THREAD_PRIORITY_MINIMUM then Priority:=THREAD_PRIORITY_MAXIMUM;

     {Select New Thread}
     while (Result = INVALID_HANDLE_VALUE) do
      begin
       {Dequeue New Thread}
       Result:=QueueDequeue(SchedulerGetQueueHandleEx(CPUID,Priority));
       if Result <> INVALID_HANDLE_VALUE then Break;

       {Decrement Priority}
       Dec(Priority);

       {Check Priority}
       if Priority < THREAD_PRIORITY_MINIMUM then Priority:=THREAD_PRIORITY_MAXIMUM;
      end;

     {Set Next Priority}
     SchedulerStarvationNext[CPUID]:=Priority;
    end
   {$IFNDEF SCHEDULER_YIELD_ALTERNATE}
   {Check Yield}
   else if Yield then
    begin
     {$IFDEF SCHEDULER_DEBUG}
     Inc(SchedulerSelectYield[CPUID]);
     {$ENDIF}

     {Get Current Priority}
     Priority:=ThreadEntry.Priority;

     {Check Priority and Queue Count (Allow for THREAD_PRIORITY_IDLE)}
     if (Priority > THREAD_PRIORITY_MINIMUM) and (not(SchedulerYieldCurrent[CPUID]) or (QueueCount(SchedulerGetQueueHandleEx(CPUID,Priority)) <= 1)) then Dec(Priority);

     {Account for THREAD_PRIORITY_NONE (IRQ/FIQ threads)}
     if Priority < THREAD_PRIORITY_MINIMUM then Priority:=FirstBitSet(SchedulerPriorityMask[CPUID]);

     {Update Yield Current}
     SchedulerYieldCurrent[CPUID]:=not(SchedulerYieldCurrent[CPUID]);

     {Select New Thread}
     while (Result = INVALID_HANDLE_VALUE) do
      begin
       {Dequeue New Thread}
       Result:=QueueDequeue(SchedulerGetQueueHandleEx(CPUID,Priority));
       if Result <> INVALID_HANDLE_VALUE then Break;

       {Decrement Priority}
       Dec(Priority);

       {Check Priority}
       if Priority < THREAD_PRIORITY_MINIMUM then Break;
      end;
    end
   {$ENDIF}
   else
    begin
     {Check Priority Mask}
     if SchedulerPriorityMask[CPUID] = 0 then
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerSelectNoMask[CPUID]);
       {$ENDIF}

       {Get Minimum Priority}
       Priority:=THREAD_PRIORITY_MINIMUM;
      end
     else
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerSelectNormal[CPUID]);
       {$ENDIF}

       {Get Highest Prioity}
       Priority:=FirstBitSet(SchedulerPriorityMask[CPUID]);

       {Select New Thread}
       while (Result = INVALID_HANDLE_VALUE) do
        begin
         {Dequeue New Thread}
         Result:=QueueDequeue(SchedulerGetQueueHandleEx(CPUID,Priority));
         if Result <> INVALID_HANDLE_VALUE then Break;

         {Get Highest Prioity}
         Priority:=FirstBitSet(SchedulerPriorityMask[CPUID]);

         {Check Priority}
         if Priority < THREAD_PRIORITY_MINIMUM then Break;
        end;
      end;
    end;

   {Check No Thread}
   if Result = INVALID_HANDLE_VALUE then
    begin
     {$IFDEF SCHEDULER_DEBUG}
     Inc(SchedulerSelectInvalid[CPUID]);
     {$ENDIF}

     {Dequeue Idle Thread}
     Result:=QueueDequeue(SchedulerGetQueueHandleEx(CPUID,THREAD_PRIORITY_IDLE));
     if Result = INVALID_HANDLE_VALUE then
      begin
       {Dequeue None Thread}
       Result:=QueueDequeue(SchedulerGetQueueHandleEx(CPUID,THREAD_PRIORITY_NONE));
      end;

     {Check No Thread}
     if Result = INVALID_HANDLE_VALUE then
      begin
       if ThreadEntry.State = THREAD_STATE_READY then
        begin
         {Restore Thread State}
         ThreadEntry.State:=THREAD_STATE_RUNNING;

         {Do not set Thread Quantum}
        end
       else
        begin
         {$IFDEF SCHEDULER_DEBUG}
         Inc(SchedulerSelectNoReady[CPUID]);
         {$ENDIF}

         {$IFDEF THREAD_DEBUG}
         if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to find ready thread, CPU Halted');
         {$ENDIF}

         Halt;
        end;
      end
     else
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerSelectDefaulted[CPUID]);
       {$ENDIF}
      end;
    end
   {Check New Thread}
   else if Result = Thread then
    begin
     {Restore Thread State}
     ThreadEntry.State:=THREAD_STATE_RUNNING;

     {Set Thread Quantum}
     SchedulerThreadQuantum[CPUID]:=SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[ThreadEntry.Priority];

     {Check Starvation Quantum (Allow for THREAD_PRIORITY_IDLE/NONE)}
     if (SchedulerStarvationQuantum[CPUID] > 0) and (ThreadEntry.Priority > THREAD_PRIORITY_MINIMUM) then
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerStarvationDecrement[CPUID]);
       {$ENDIF}

       {Decrement Starvation Quantum}
       Dec(SchedulerStarvationQuantum[CPUID]);
      end;
    end
   else
    begin
     {Set Thread Quantum}
     SchedulerThreadQuantum[CPUID]:=SCHEDULER_THREAD_QUANTUM + SCHEDULER_PRIORITY_QUANTUM[Priority];

     {Check Starvation Quantum (Allow for thread priority)}
     if (SchedulerStarvationQuantum[CPUID] > 0) and (Priority > THREAD_PRIORITY_NORMAL) and (Priority >= ThreadEntry.Priority) then
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerStarvationDecrement[CPUID]);
       {$ENDIF}

       {Decrement Starvation Quantum}
       Dec(SchedulerStarvationQuantum[CPUID]);
      end
     else if (SchedulerStarvationQuantum[CPUID] < SCHEDULER_STARVATION_QUANTUM) and (Priority <= THREAD_PRIORITY_MINIMUM) then
      begin
       {$IFDEF SCHEDULER_DEBUG}
       Inc(SchedulerStarvationReset[CPUID]);
       {$ENDIF}

       {Reset Starvation Quantum}
       SchedulerStarvationQuantum[CPUID]:=SCHEDULER_STARVATION_QUANTUM;
      end;
    end;
  end;
end;

{==============================================================================}

function SchedulerReschedule(Yield:Boolean):LongWord;
{Perform a thread switch operation when a thread yields, sleeps or waits

 The next thread to run will be selected based on whether the current thread is
 yielding or no longer ready, remaining quantum of the current thread, ready
 threads at higher priority levels and scheduler priority quantum for fair
 scheduling of lower priority threads}
{Yield: True if the current thread is giving up its remaining time slice}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Called by the currently running thread to force a reschedule before sleeping, waiting etc}
var
 IRQMask:TIRQMask;
 FIQMask:TFIQMask;
 CurrentCPU:LongWord;

 Thread:TThreadHandle;
 NewThread:TThreadHandle;

 ThreadEntry:PThreadEntry;
 NewThreadEntry:PThreadEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF SCHEDULER_DEBUG}
 Inc(SchedulerRescheduleEntry[CPUGetCurrent]);
 {$ENDIF}

 {Check the Handler}
 if Assigned(SchedulerRescheduleHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerRescheduleHandler(Yield);
  end
 else
  begin
   {Use the Default method}
   {Get Current CPU}
   CurrentCPU:=CPUGetCurrent;

   {Get Current Thread}
   Thread:=ThreadGetCurrent;
   if Thread = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   ThreadEntry:=PThreadEntry(Thread);
   if ThreadEntry = nil then Exit;
   if ThreadEntry.Signature <> THREAD_SIGNATURE then Exit;

   {Disable IRQ/FIQ}
   if SCHEDULER_FIQ_ENABLED then
    begin
     FIQMask:=SaveIRQFIQ;
    end
   else
    begin
     IRQMask:=SaveIRQ;
    end;
   try
    {Select New Thread}
    NewThread:=SchedulerSelect(CurrentCPU,Thread,Yield);
    if NewThread <> INVALID_HANDLE_VALUE then
     begin
      {Check the Handle}
      NewThreadEntry:=PThreadEntry(NewThread);
      if NewThreadEntry = nil then Exit;
      if NewThreadEntry.Signature <> THREAD_SIGNATURE then Exit;
     end;

    {$IFDEF SCHEDULER_DEBUG}
    SchedulerRescheduleThread[CurrentCPU]:=NewThread;
    {$ENDIF}

    {Check New Thread}
    if (NewThread = Thread) or (NewThread = INVALID_HANDLE_VALUE)  then
     begin
      {Scheduler Select handles state if no selection or same selection}
      {$IFDEF SCHEDULER_DEBUG}
      if NewThread = INVALID_HANDLE_VALUE then
       begin
        Inc(SchedulerRescheduleInvalid[CurrentCPU]);
       end
      else
       begin
        Inc(SchedulerRescheduleCurrent[CurrentCPU]);
       end;
      {$ENDIF}
     end
    else
     begin
      {$IFDEF SCHEDULER_DEBUG}
      Inc(SchedulerRescheduleCounter[CurrentCPU]);
      {$ENDIF}

      {Set State}
      NewThreadEntry.State:=THREAD_STATE_RUNNING;

      {Set CPU}
      NewThreadEntry.CurrentCPU:=CurrentCPU;

      {$IFDEF THREAD_STATISTICS}
      {Update Switch Count}
      Inc(NewThreadEntry.SwitchCount);
      {$ENDIF}

      if SCHEDULER_SWI_ENABLED then
       begin
        {Perform the System Call}
        SystemCall(SYSTEM_CALL_CONTEXT_SWITCH,PtrUInt(@ThreadEntry.StackPointer),PtrUInt(@NewThreadEntry.StackPointer),NewThread);
       end
      else
       begin
        {Perform the Context Switch}
        ContextSwitch(@ThreadEntry.StackPointer,@NewThreadEntry.StackPointer,NewThread);
       end;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Restore IRQ/FIQ}
    if SCHEDULER_FIQ_ENABLED then
     begin
      RestoreIRQFIQ(FIQMask);
     end
    else
     begin
      RestoreIRQ(IRQMask);
     end;
   end;
  end;
end;

{==============================================================================}

function SchedulerMigrationEnable:LongWord;
{Enable scheduler thread migration}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Handler}
 if Assigned(SchedulerMigrationEnableHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerMigrationEnableHandler;
  end
 else
  begin
   {Use the Default method}
   SchedulerThreadMigration:=SCHEDULER_MIGRATION_ENABLED;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SchedulerMigrationDisable:LongWord;
{Disable scheduler thread migration}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Handler}
 if Assigned(SchedulerMigrationDisableHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerMigrationDisableHandler;
  end
 else
  begin
   {Use the Default method}
   SchedulerThreadMigration:=SCHEDULER_MIGRATION_DISABLED;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SchedulerPreemptEnable(CPUID:LongWord):LongWord;
{Enable thread preemption for the specified CPU}
{CPUID: The ID of the CPU to enable for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check the Handler}
 if Assigned(SchedulerPreemptEnableHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerPreemptEnableHandler(CPUID);
  end
 else
  begin
   {Use the Default method}
   SchedulerThreadPreempt[CPUID]:=SCHEDULER_PREEMPT_ENABLED;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SchedulerPreemptDisable(CPUID:LongWord):LongWord;
{Disable thread preemption for the specified CPU}
{CPUID: The ID of the CPU to disable for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CPU}
 {if CPUID = SCHEDULER_CPU_BOOT then Exit;} {Cannot disable on Boot CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check the Handler}
 if Assigned(SchedulerPreemptDisableHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerPreemptDisableHandler(CPUID);
  end
 else
  begin
   {Use the Default method}
   SchedulerThreadPreempt[CPUID]:=SCHEDULER_PREEMPT_DISABLED;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SchedulerAllocationEnable(CPUID:LongWord):LongWord;
{Enable thread allocation for the specified CPU}
{CPUID: The ID of the CPU to enable for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check the Handler}
 if Assigned(SchedulerAllocationEnableHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerAllocationEnableHandler(CPUID);
  end
 else
  begin
   {Use the Default method}
   SchedulerThreadAllocation[CPUID]:=SCHEDULER_ALLOCATION_ENABLED;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function SchedulerAllocationDisable(CPUID:LongWord):LongWord;
{Disable thread allocation for the specified CPU}
{CPUID: The ID of the CPU to disable for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check CPU}
 if CPUID = SCHEDULER_CPU_BOOT then Exit; {Cannot disable on Boot CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check the Handler}
 if Assigned(SchedulerAllocationDisableHandler) then
  begin
   {Use the Handler method}
   Result:=SchedulerAllocationDisableHandler(CPUID);
  end
 else
  begin
   {Use the Default method}
   SchedulerThreadAllocation[CPUID]:=SCHEDULER_ALLOCATION_DISABLED;

   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}
{==============================================================================}
{Messageslot Functions}
function MessageslotCreate:TMessageslotHandle; {$IFDEF MESSAGESLOT_INLINE}inline;{$ENDIF}
{Create and insert a new Messageslot entry}
{Return: Handle of new Messageslot entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=MessageslotCreateEx(MESSAGESLOT_DEFAULT_MAXIMUM,MESSAGESLOT_FLAG_NONE);
end;

{==============================================================================}

function MessageslotCreateEx(Maximum:LongWord;Flags:LongWord):TMessageslotHandle;
{Create and insert a new Messageslot entry}
{Maximum: Maximum number of messages allowed for the Messageslot (Must be greater than zero)}
{Flags: The flags for the Messageslot entry (eg MESSAGESLOT_FLAG_IRQ)}
{Return: Handle of new Messageslot entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 MessageslotEntry:PMessageslotEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Messageslot Create (Maximum=' + IntToStr(Maximum) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Maximum}
 if Maximum < 1 then Exit;

 {Create Messageslot entry}
 if MESSAGESLOT_SHARED_MEMORY then
  begin
   MessageslotEntry:=AllocSharedMem(SizeOf(TMessageslotEntry));
  end
 else
  begin
   MessageslotEntry:=AllocMem(SizeOf(TMessageslotEntry));
  end;
 if MessageslotEntry = nil then Exit;

 {Setup Messageslot entry}
 MessageslotEntry.Signature:=MESSAGESLOT_SIGNATURE;
 MessageslotEntry.Flags:=Flags;
 MessageslotEntry.Lock:=SpinCreate;
 MessageslotEntry.List:=INVALID_HANDLE_VALUE;
 MessageslotEntry.Messages.Maximum:=Maximum;
 MessageslotEntry.Messages.List:=AllocMem(SizeOf(TMessage) * MessageslotEntry.Messages.Maximum);
 MessageslotEntry.Wait:=ThreadWait;
 MessageslotEntry.WaitEx:=ThreadWaitEx;
 MessageslotEntry.Release:=ThreadRelease;
 MessageslotEntry.Abandon:=ThreadAbandon;

 {Check Messageslot flags}
 if (Flags and (MESSAGESLOT_FLAG_IRQ or MESSAGESLOT_FLAG_FIQ or MESSAGESLOT_FLAG_IRQFIQ)) <> 0 then
  begin
   {Create Messageslot List}
   MessageslotEntry.List:=ListCreateEx(LIST_TYPE_WAIT_MESSAGESLOT,SchedulerGetListFlags(LIST_TYPE_WAIT_MESSAGESLOT));
  end;

 {Insert Messageslot entry}
 if SpinLock(MessageslotTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Messageslot entry}
    if MessageslotTable = nil then
     begin
      MessageslotTable:=MessageslotEntry;
     end
    else
     begin
      MessageslotEntry.Next:=MessageslotTable;
      MessageslotTable.Prev:=MessageslotEntry;
      MessageslotTable:=MessageslotEntry;
     end;

    {Increment Messageslot Count}
    Inc(MessageslotTableCount);

    {Return Messageslot entry}
    Result:=TMessageslotHandle(MessageslotEntry);
   finally
    SpinUnlock(MessageslotTableLock);
   end;
  end
 else
  begin
   {Free Messageslot List}
   if MessageslotEntry.List <> INVALID_HANDLE_VALUE then
    begin
     ListDestroy(MessageslotEntry.List);
    end;

   {Free Messageslot Lock}
   SpinDestroy(MessageslotEntry.Lock);

   {Free Messageslot Entry}
   FreeMem(MessageslotEntry);
  end;
end;

{==============================================================================}

function MessageslotDestroy(Messageslot:TMessageslotHandle):LongWord;
{Destroy and remove an existing Messageslot entry}
{Messageslot: Handle of Messageslot entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MessageslotEntry:PMessageslotEntry;
 PrevEntry:PMessageslotEntry;
 NextEntry:PMessageslotEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Messageslot Destroy (Handle=' + HandleToHex(Messageslot) + ')');
 {$ENDIF}

 {Check Messageslot}
 if Messageslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MessageslotEntry:=PMessageslotEntry(Messageslot);
 if MessageslotEntry = nil then Exit;
 if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

 {Remove Messageslot entry}
 if SpinLock(MessageslotTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinLockIRQFIQ(MessageslotEntry.Lock);
     end
    else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinLockFIQ(MessageslotEntry.Lock);
     end
    else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinLockIRQ(MessageslotEntry.Lock);
     end
    else
     begin
      Result:=SpinLock(MessageslotEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then
     begin
      {Release the Lock}
      if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
       begin
        Result:=SpinUnlockIRQFIQ(MessageslotEntry.Lock);
       end
      else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
       begin
        Result:=SpinUnlockFIQ(MessageslotEntry.Lock);
       end
      else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
       begin
        Result:=SpinUnlockIRQ(MessageslotEntry.Lock);
       end
      else
       begin
        Result:=SpinUnlock(MessageslotEntry.Lock);
       end;
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Messageslot entry}
    MessageslotEntry.Signature:=0;

    {Check List}
    while ListNotEmpty(MessageslotEntry.List) do
     begin
      {Abandon waiting thread}
      MessageslotEntry.Abandon(MessageslotEntry.List);
     end;

    {Unlink Messageslot entry}
    PrevEntry:=MessageslotEntry.Prev;
    NextEntry:=MessageslotEntry.Next;
    if PrevEntry = nil then
     begin
      MessageslotTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Messageslot Count}
    Dec(MessageslotTableCount);

    {Release the Lock}
    if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
     begin
      Result:=SpinUnlockIRQFIQ(MessageslotEntry.Lock);
     end
    else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
     begin
      Result:=SpinUnlockFIQ(MessageslotEntry.Lock);
     end
    else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
     begin
      Result:=SpinUnlockIRQ(MessageslotEntry.Lock);
     end
    else
     begin
      Result:=SpinUnlock(MessageslotEntry.Lock);
     end;
    if Result <> ERROR_SUCCESS then Exit;

    {Free Messageslot List}
    if MessageslotEntry.List <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(MessageslotEntry.List);
     end;

    {Free Messageslot Lock}
    SpinDestroy(MessageslotEntry.Lock);

    {Free Messageslot Entry}
    FreeMem(MessageslotEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(MessageslotTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MessageslotCount(Messageslot:TMessageslotHandle):LongWord;
{Get the number of available messages in a Messageslot entry}
{Messageslot: Messageslot to get from}
{Return: Number of messages or INVALID_HANDLE_VALUE on error}
var
 ResultCode:LongWord;
 MessageslotEntry:PMessageslotEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Messageslot Count (Handle=' + HandleToHex(Messageslot) + ')');
 {$ENDIF}

 {Check Messageslot}
 if Messageslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MessageslotEntry:=PMessageslotEntry(Messageslot);
 if MessageslotEntry = nil then Exit;
 if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

 {Lock the Messageslot}
 if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQFIQ(MessageslotEntry.Lock);
  end
 else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
  begin
   ResultCode:=SpinLockFIQ(MessageslotEntry.Lock);
  end
 else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQ(MessageslotEntry.Lock);
  end
 else
  begin
   ResultCode:=SpinLock(MessageslotEntry.Lock);
  end;
 {Check Lock Result}
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

    {Get the Count}
    Result:=MessageslotEntry.Messages.Count;
   finally
    {Unlock the Messageslot}
    if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
     begin
      SpinUnlockIRQFIQ(MessageslotEntry.Lock);
     end
    else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
     begin
      SpinUnlockFIQ(MessageslotEntry.Lock);
     end
    else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
     begin
      SpinUnlockIRQ(MessageslotEntry.Lock);
     end
    else
     begin
      SpinUnlock(MessageslotEntry.Lock);
     end;
   end;
  end;
end;

{==============================================================================}

function MessageslotSend(Messageslot:TMessageslotHandle;const Message:TMessage):LongWord;
{Send a message to a Messageslot}
{Messageslot: Messageslot to send to}
{Message: Contents of message to send}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ResultCode:LongWord;
 MessageslotEntry:PMessageslotEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Messageslot Send Message (Handle=' + HandleToHex(Messageslot) + ')');
 {$ENDIF}

 {Check Messageslot}
 if Messageslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MessageslotEntry:=PMessageslotEntry(Messageslot);
 if MessageslotEntry = nil then Exit;
 if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(MessageslotSendHandler) then
  begin
   {Use the Handler method}
   Result:=MessageslotSendHandler(MessageslotEntry,@Message);
  end
 else
  begin
   {Use the Default method}
   {Lock the Messageslot}
   if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQFIQ(MessageslotEntry.Lock);
    end
   else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
    begin
     ResultCode:=SpinLockFIQ(MessageslotEntry.Lock);
    end
   else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQ(MessageslotEntry.Lock);
    end
   else
    begin
     {$IFDEF LOCK_DEBUG}
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(MessageslotDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(MessageslotDeadlockCounter);
        end;
      end;
     {$ENDIF}

     ResultCode:=SpinLock(MessageslotEntry.Lock);
    end;
   {Check Lock Result}
   if ResultCode = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

      {Check Count}
      if MessageslotEntry.Messages.Count >= MessageslotEntry.Messages.Maximum then
       begin
        Result:=ERROR_NO_MORE_ITEMS;
        Exit;
       end;

      {Write Message}
      PMessage(PtrUInt(MessageslotEntry.Messages.List) + PtrUInt(((MessageslotEntry.Messages.Start + MessageslotEntry.Messages.Count) mod MessageslotEntry.Messages.Maximum) * SizeOf(TMessage)))^:=Message;

      {Update Count}
      Inc(MessageslotEntry.Messages.Count);

      {Check List}
      while ListNotEmpty(MessageslotEntry.List) do
       begin
        {Release one thread waiting on Messageslot}
        if MessageslotEntry.Release(MessageslotEntry.List) = ERROR_SUCCESS then
         begin
          Break;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Messageslot}
      if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
       begin
        SpinUnlockIRQFIQ(MessageslotEntry.Lock);
       end
      else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
       begin
        SpinUnlockFIQ(MessageslotEntry.Lock);
       end
      else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
       begin
        SpinUnlockIRQ(MessageslotEntry.Lock);
       end
      else
       begin
        SpinUnlock(MessageslotEntry.Lock);
       end;
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function MessageslotReceive(Messageslot:TMessageslotHandle;var Message:TMessage):LongWord;
{Receive a message from a Messageslot}
{Messageslot: Messageslot to receive from}
{Message: The received message if successful, undefined on error}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MessageslotEntry:PMessageslotEntry;
begin
 {}
 {Check the Handler}
 if Assigned(MessageslotReceiveHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check Messageslot}
   if Messageslot = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   MessageslotEntry:=PMessageslotEntry(Messageslot);
   if MessageslotEntry = nil then Exit;
   if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

   {Use the Handler method}
   Result:=MessageslotReceiveHandler(MessageslotEntry,@Message);
  end
 else
  begin
   {Use the Default method}
   Result:=MessageslotReceiveEx(Messageslot,Message,INFINITE);
  end;
end;

{==============================================================================}

function MessageslotReceiveEx(Messageslot:TMessageslotHandle;var Message:TMessage;Timeout:LongWord):LongWord;
{Receive a message from a Messageslot}
{Messageslot: Messageslot to receive from}
{Message: The received message if successful, undefined on error}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 ResultCode:LongWord;
 MessageslotEntry:PMessageslotEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IFDEF THREAD_DEBUG}
 if THREAD_LOG_ENABLED then ThreadLogDebug('Messageslot Receive Message (Handle=' + HandleToHex(Messageslot) + ')');
 {$ENDIF}

 {Check Messageslot}
 if Messageslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MessageslotEntry:=PMessageslotEntry(Messageslot);
 if MessageslotEntry = nil then Exit;
 if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(MessageslotReceiveExHandler) then
  begin
   {Use the Handler method}
   Result:=MessageslotReceiveExHandler(MessageslotEntry,@Message,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Messageslot}
   if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQFIQ(MessageslotEntry.Lock);
    end
   else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
    begin
     ResultCode:=SpinLockFIQ(MessageslotEntry.Lock);
    end
   else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQ(MessageslotEntry.Lock);
    end
   else
    begin
     {$IFDEF LOCK_DEBUG}
     if SCHEDULER_FIQ_ENABLED then
      begin
       if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(MessageslotDeadlockCounter);
        end;
      end
     else
      begin
       if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
        begin
         Inc(MessageslotDeadlockCounter);
        end;
      end;
     {$ENDIF}

     ResultCode:=SpinLock(MessageslotEntry.Lock);
    end;
   {Check Lock Result}
   if ResultCode = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if MessageslotEntry.Signature <> MESSAGESLOT_SIGNATURE then Exit;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check Count}
        Result:=ERROR_WAIT_TIMEOUT;
        if MessageslotEntry.Messages.Count = 0 then Exit;
       end;

      {Check for Message}
      if MessageslotEntry.Messages.Count > 0 then
       begin
        {Read Message}
        Message:=PMessage(PtrUInt(MessageslotEntry.Messages.List) + PtrUInt(MessageslotEntry.Messages.Start * SizeOf(TMessage)))^;

        {Update Start}
        MessageslotEntry.Messages.Start:=(MessageslotEntry.Messages.Start + 1) mod MessageslotEntry.Messages.Maximum;

        {Update Count}
        Dec(MessageslotEntry.Messages.Count);
       end
      else
       begin
        {Check List}
        if MessageslotEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          MessageslotEntry.List:=ListCreateEx(LIST_TYPE_WAIT_MESSAGESLOT,SchedulerGetListFlags(LIST_TYPE_WAIT_MESSAGESLOT));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Message}
          MessageslotEntry.Wait(MessageslotEntry.List,MessageslotEntry.Lock,MessageslotEntry.Flags);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;

          {Receive Message (Infinite Wait)}
          Result:=MessageslotReceiveEx(Messageslot,Message,INFINITE);
          Exit;
         end
        else
         begin
          {Wait on Event with Timeout}
          MessageslotEntry.WaitEx(MessageslotEntry.List,MessageslotEntry.Lock,MessageslotEntry.Flags,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;

          {Receive Message (No Wait)}
          Result:=MessageslotReceiveEx(Messageslot,Message,0);
          Exit;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Messageslot}
      if Unlock then
       begin
        if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQFIQ) <> 0 then
         begin
          SpinUnlockIRQFIQ(MessageslotEntry.Lock);
         end
        else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_FIQ) <> 0 then
         begin
          SpinUnlockFIQ(MessageslotEntry.Lock);
         end
        else if (MessageslotEntry.Flags and MESSAGESLOT_FLAG_IRQ) <> 0 then
         begin
          SpinUnlockIRQ(MessageslotEntry.Lock);
         end
        else
         begin
          SpinUnlock(MessageslotEntry.Lock);
         end;
       end;
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Mailslot Functions}
function MailslotCreate(Maximum:LongWord):TMailslotHandle;
{Create and insert a new Mailslot entry}
{Maximum: Maximum number of messages allowed for the Mailslot (Must be greater than zero)}
{Return: Handle of new Mailslot entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 MailslotEntry:PMailslotEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Maximum}
 if Maximum < 1 then Exit;

 {Create Mailslot entry}
 if MAILSLOT_SHARED_MEMORY then
  begin
   MailslotEntry:=AllocSharedMem(SizeOf(TMailslotEntry));
  end
 else
  begin
   MailslotEntry:=AllocMem(SizeOf(TMailslotEntry));
  end;
 if MailslotEntry = nil then Exit;

 {Setup Mailslot entry}
 MailslotEntry.Signature:=MAILSLOT_SIGNATURE;
 MailslotEntry.Maximum:=Maximum;
 MailslotEntry.Count:=0;
 MailslotEntry.Start:=0;
 MailslotEntry.Lock:=SpinCreate;
 MailslotEntry.Sender:=SemaphoreCreate(Maximum);
 MailslotEntry.Receiver:=SemaphoreCreate(0);
 MailslotEntry.Messages:=AllocMem(Maximum * SizeOf(PtrInt));

 {Check Mailslot entry}
 if (MailslotEntry.Sender = INVALID_HANDLE_VALUE) or (MailslotEntry.Receiver = INVALID_HANDLE_VALUE) or (MailslotEntry.Messages = nil) then
  begin
   {Free Mailslot Semaphores}
   if MailslotEntry.Receiver <> INVALID_HANDLE_VALUE then SemaphoreDestroy(MailslotEntry.Receiver);
   if MailslotEntry.Sender <> INVALID_HANDLE_VALUE then SemaphoreDestroy(MailslotEntry.Sender);

   {Free Mailslot Messages}
   if MailslotEntry.Messages <> nil then FreeMem(MailslotEntry.Messages);

   {Free Mailslot Lock}
   SpinDestroy(MailslotEntry.Lock);

   {Free Mailslot Entry}
   FreeMem(MailslotEntry);

   Exit;
  end;

 {Insert Mailslot entry}
 if SpinLock(MailslotTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Mailslot entry}
    if MailslotTable = nil then
     begin
      MailslotTable:=MailslotEntry;
     end
    else
     begin
      MailslotEntry.Next:=MailslotTable;
      MailslotTable.Prev:=MailslotEntry;
      MailslotTable:=MailslotEntry;
     end;

    {Increment Mailslot Count}
    Inc(MailslotTableCount);

    {Return Mailslot entry}
    Result:=TMailslotHandle(MailslotEntry);
   finally
    SpinUnlock(MailslotTableLock);
   end;
  end
 else
  begin
   {Free Mailslot Semaphores}
   SemaphoreDestroy(MailslotEntry.Receiver);
   SemaphoreDestroy(MailslotEntry.Sender);

   {Free Mailslot Messages}
   FreeMem(MailslotEntry.Messages);

   {Free Mailslot Lock}
   SpinDestroy(MailslotEntry.Lock);

   {Free Mailslot Entry}
   FreeMem(MailslotEntry);
  end;
end;

{==============================================================================}

function MailslotDestroy(Mailslot:TMailslotHandle):LongWord;
{Destroy and remove an existing Mailslot entry}
{Mailslot: Handle of Mailslot entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MailslotEntry:PMailslotEntry;
 PrevEntry:PMailslotEntry;
 NextEntry:PMailslotEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Mailslot}
 if Mailslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MailslotEntry:=PMailslotEntry(Mailslot);
 if MailslotEntry = nil then Exit;
 if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

 {Remove Mailslot entry}
 if SpinLock(MailslotTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=SpinLock(MailslotEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then
     begin
      {Release the Lock}
      Result:=SpinUnlock(MailslotEntry.Lock);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Mailslot entry}
    MailslotEntry.Signature:=0;

    {Unlink Mailslot entry}
    PrevEntry:=MailslotEntry.Prev;
    NextEntry:=MailslotEntry.Next;
    if PrevEntry = nil then
     begin
      MailslotTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Mailslot Count}
    Dec(MailslotTableCount);

    {Release the Lock}
    Result:=SpinUnlock(MailslotEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Free Mailslot Semaphores}
    SemaphoreDestroy(MailslotEntry.Receiver);
    SemaphoreDestroy(MailslotEntry.Sender);

    {Free Mailslot Messages}
    FreeMem(MailslotEntry.Messages);

    {Free Mailslot Lock}
    SpinDestroy(MailslotEntry.Lock);

    {Free Mailslot Entry}
    FreeMem(MailslotEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(MailslotTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function MailslotCount(Mailslot:TMailslotHandle):LongWord;
{Get the number of available messages in a Mailslot entry}
{Mailslot: Mailslot to get from}
{Return: Number of messages or INVALID_HANDLE_VALUE on error}
var
 MailslotEntry:PMailslotEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Mailslot}
 if Mailslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MailslotEntry:=PMailslotEntry(Mailslot);
 if MailslotEntry = nil then Exit;
 if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

 {Acquire the Lock}
 if SpinLock(MailslotEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

    {Return Count}
    Result:=MailslotEntry.Count;
   finally
    {Release the Lock}
    SpinUnlock(MailslotEntry.Lock);
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function MailslotSend(Mailslot:TMailslotHandle;Data:PtrInt):LongWord;
{Send a message to a Mailslot}
{Mailslot: Mailslot to send to}
{Data: Message to send to mailslot}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MailslotEntry:PMailslotEntry;
begin
 {}
 {Check the Handler}
 if Assigned(MailslotSendHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check Mailslot}
   if Mailslot = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   MailslotEntry:=PMailslotEntry(Mailslot);
   if MailslotEntry = nil then Exit;
   if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

   {$IFDEF LOCK_DEBUG}
   if SCHEDULER_FIQ_ENABLED then
    begin
     if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(MailslotDeadlockCounter);
      end;
    end
   else
    begin
     if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(MailslotDeadlockCounter);
      end;
    end;
   {$ENDIF}

   {Use the Handler method}
   Result:=MailslotSendHandler(MailslotEntry,Data);
  end
 else
  begin
   {Use the Default method}
   Result:=MailslotSendEx(Mailslot,Data,INFINITE);
  end;
end;

{==============================================================================}

function MailslotSendEx(Mailslot:TMailslotHandle;Data:PtrInt;Timeout:LongWord):LongWord;
{Send a message to a Mailslot}
{Mailslot: Mailslot to send to}
{Data: Message to send to mailslot}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MailslotEntry:PMailslotEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Mailslot}
 if Mailslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MailslotEntry:=PMailslotEntry(Mailslot);
 if MailslotEntry = nil then Exit;
 if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(MailslotDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(MailslotDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(MailslotSendExHandler) then
  begin
   {Use the Handler method}
   Result:=MailslotSendExHandler(MailslotEntry,Data,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Wait for room to send}
   Result:=SemaphoreWaitEx(MailslotEntry.Sender,Timeout);
   if Result = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if SpinLock(MailslotEntry.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Signature}
        Result:=ERROR_INVALID_PARAMETER;
        if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

        {Write the message to the Mailslot}
        PPtrInt(PtrUInt(MailslotEntry.Messages) + (((MailslotEntry.Start + MailslotEntry.Count) mod MailslotEntry.Maximum) * SizeOf(PtrInt)))^:=Data;

        {Update the Mailslot}
        Inc(MailslotEntry.Count);

        {Signal a received message}
        SemaphoreSignal(MailslotEntry.Receiver);

        {Return Result}
        Result:=ERROR_SUCCESS;
       finally
        {Release the Lock}
        SpinUnlock(MailslotEntry.Lock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
      end;
    end;
  end;
end;

{==============================================================================}

function MailslotReceive(Mailslot:TMailslotHandle):PtrInt;
{Receive a message from a Mailslot}
{Mailslot: Mailslot to receive from}
{Return: Received message or INVALID_HANDLE_VALUE on error}
var
 MailslotEntry:PMailslotEntry;
begin
 {}
 {Check the Handler}
 if Assigned(MailslotReceiveHandler) then
  begin
   Result:=INVALID_HANDLE_VALUE;

   {Check Mailslot}
   if Mailslot = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   MailslotEntry:=PMailslotEntry(Mailslot);
   if MailslotEntry = nil then Exit;
   if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

   {$IFDEF LOCK_DEBUG}
   if SCHEDULER_FIQ_ENABLED then
    begin
     if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(MailslotDeadlockCounter);
      end;
    end
   else
    begin
     if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(MailslotDeadlockCounter);
      end;
    end;
   {$ENDIF}

   {Use the Handler method}
   Result:=MailslotReceiveHandler(MailslotEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=MailslotReceiveEx(Mailslot,INFINITE);
  end;
end;

{==============================================================================}

function MailslotReceiveEx(Mailslot:TMailslotHandle;Timeout:LongWord):PtrInt;
{Receive a message from a Mailslot}
{Mailslot: Mailslot to receive from}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: Received message or INVALID_HANDLE_VALUE on error}
var
 MailslotEntry:PMailslotEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Mailslot}
 if Mailslot = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 MailslotEntry:=PMailslotEntry(Mailslot);
 if MailslotEntry = nil then Exit;
 if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(MailslotDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(MailslotDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(MailslotReceiveExHandler) then
  begin
   {Use the Handler method}
   Result:=MailslotReceiveExHandler(MailslotEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Wait for a received message}
   if SemaphoreWaitEx(MailslotEntry.Receiver,Timeout) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if SpinLock(MailslotEntry.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Signature}
        Result:=ERROR_INVALID_PARAMETER;
        if MailslotEntry.Signature <> MAILSLOT_SIGNATURE then Exit;

        {Receive the first message}
        Result:=PPtrInt(PtrUInt(MailslotEntry.Messages) + (MailslotEntry.Start * SizeOf(PtrInt)))^;

        {Update the Mailslot}
        MailslotEntry.Start:=(MailslotEntry.Start + 1) mod MailslotEntry.Maximum;
        Dec(MailslotEntry.Count);

        {Signal room to send}
        SemaphoreSignal(MailslotEntry.Sender);
       finally
        {Release the Lock}
        SpinUnlock(MailslotEntry.Lock);
       end;
      end
     else
      begin
       {Nothing}
      end;
    end
   else
    begin
     {Nothing}
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Buffer Functions}
function BufferCreate(Size,Count:LongWord):TBufferHandle; {$IFDEF BUFFER_INLINE}inline;{$ENDIF}
{Create and insert a new Buffer entry}
{Size: Size of each buffer in bytes}
{Count: Total number of buffers}
{Return: Handle of new Buffer entry or INVALID_HANDLE_VALUE if entry could not be created}
begin
 {}
 Result:=BufferCreateEx(Size,Count,BUFFER_FLAG_NONE);
end;

{==============================================================================}

function BufferCreateEx(Size,Count,Flags:LongWord):TBufferHandle;
{Create and insert a new Buffer entry}
{Size: Size of each buffer in bytes}
{Count: Total number of buffers}
{Flags: Flags for buffer (eg BUFFER_FLAG_SHARED)}
{Return: Handle of new Buffer entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 BufferSize:LongWord;
 BufferCount:LongWord;
 BufferItem:PBufferItem;
 BufferEntry:PBufferEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Adjust Size}
 BufferSize:=Align(Size,HEAP_MIN_ALIGNMENT) + SizeOf(TBufferItem);

 {Check Size}
 if BufferSize < BUFFER_MIN_SIZE then Exit;
 if BufferSize > BUFFER_MAX_SIZE then Exit;

 {Check Count}
 if Count < 1 then Exit;
 if Count > BUFFER_MAX_COUNT then Exit;

 {Create Buffer entry}
 if BUFFER_SHARED_MEMORY then
  begin
   BufferEntry:=AllocSharedMem(SizeOf(TBufferEntry));
  end
 else
  begin
   BufferEntry:=AllocMem(SizeOf(TBufferEntry));
  end;
 if BufferEntry = nil then Exit;

 {Setup Buffer entry}
 BufferEntry.Signature:=BUFFER_SIGNATURE;
 BufferEntry.Size:=BufferSize;
 BufferEntry.Count:=Count;
 BufferEntry.Flags:=Flags;
 BufferEntry.Lock:=SpinCreate;
 BufferEntry.Available:=SemaphoreCreate(Count);
 if (BufferEntry.Flags and BUFFER_FLAG_SHARED) <> 0 then
  begin
   BufferEntry.Buffers:=AllocSharedMem(Count * BufferSize);
  end
 else
  begin
   BufferEntry.Buffers:=AllocMem(Count * BufferSize);
  end;

 {Check Buffer entry}
 if (BufferEntry.Available = INVALID_HANDLE_VALUE) or (BufferEntry.Buffers = nil) then
  begin
   {Free Buffer Semaphore}
   if BufferEntry.Available <> INVALID_HANDLE_VALUE then SemaphoreDestroy(BufferEntry.Available);

   {Free Buffer Buffers}
   if BufferEntry.Buffers <> nil then FreeMem(BufferEntry.Buffers);

   {Free Buffer Lock}
   SpinDestroy(BufferEntry.Lock);

   {Free Buffer Entry}
   FreeMem(BufferEntry);

   Exit;
  end;

 {Initialize Buffer items}
 BufferItem:=BufferEntry.Buffers;
 BufferEntry.First:=BufferItem;
 for BufferCount:=0 to Count - 1 do
  begin
   {Initialize Item}
   BufferItem.Parent:=TBufferHandle(BufferEntry);
   BufferItem.Next:=PBufferItem(PtrUInt(BufferItem) + PtrUInt(BufferSize));
   BufferItem.Buffer:=Pointer(PtrUInt(BufferItem) + PtrUInt(SizeOf(TBufferItem)));

   {Get Next Item}
   if BufferCount < Count - 1 then BufferItem:=BufferItem.Next;
  end;
 {Update Last Item}
 BufferItem.Next:=nil;

 {Insert Buffer entry}
 if SpinLock(BufferTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Buffer entry}
    if BufferTable = nil then
     begin
      BufferTable:=BufferEntry;
     end
    else
     begin
      BufferEntry.Next:=BufferTable;
      BufferTable.Prev:=BufferEntry;
      BufferTable:=BufferEntry;
     end;

    {Increment Buffer Count}
    Inc(BufferTableCount);

    {Return Buffer entry}
    Result:=TBufferHandle(BufferEntry);
   finally
    SpinUnlock(BufferTableLock);
   end;
  end
 else
  begin
   {Free Buffer Semaphore}
   SemaphoreDestroy(BufferEntry.Available);

   {Free Buffer Buffers}
   FreeMem(BufferEntry.Buffers);

   {Free Buffer Lock}
   SpinDestroy(BufferEntry.Lock);

   {Free Buffer Entry}
   FreeMem(BufferEntry);
  end;
end;

{==============================================================================}

function BufferDestroy(Buffer:TBufferHandle):LongWord;
{Destroy and remove an existing Buffer entry}
{Buffer: Handle of Buffer entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 BufferEntry:PBufferEntry;
 PrevEntry:PBufferEntry;
 NextEntry:PBufferEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 BufferEntry:=PBufferEntry(Buffer);
 if BufferEntry = nil then Exit;
 if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

 {Remove Buffer entry}
 if SpinLock(BufferTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=SpinLock(BufferEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if BufferEntry.Signature <> BUFFER_SIGNATURE then
     begin
      {Release the Lock}
      Result:=SpinUnlock(BufferEntry.Lock);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Buffer entry}
    BufferEntry.Signature:=0;

    {Unlink Buffer entry}
    PrevEntry:=BufferEntry.Prev;
    NextEntry:=BufferEntry.Next;
    if PrevEntry = nil then
     begin
      BufferTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Buffer Count}
    Dec(BufferTableCount);

    {Release the Lock}
    Result:=SpinUnlock(BufferEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Free Buffer Semaphores}
    SemaphoreDestroy(BufferEntry.Available);

    {Free Buffer Messages}
    FreeMem(BufferEntry.Buffers);

    {Free Buffer Lock}
    SpinDestroy(BufferEntry.Lock);

    {Free Buffer Entry}
    FreeMem(BufferEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(BufferTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BufferCount(Buffer:TBufferHandle):LongWord;
{Get the total count of buffers in an existing Buffer entry}
{Buffer: Buffer to get total count for}
{Return: Total count or INVALID_HANDLE_VALUE on error}
var
 BufferEntry:PBufferEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Buffer}
 if Buffer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 BufferEntry:=PBufferEntry(Buffer);
 if BufferEntry = nil then Exit;
 if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

 {Acquire the Lock}
 if SpinLock(BufferEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

    {Get Total Count}
    Result:=BufferEntry.Count;
   finally
    {Release the Lock}
    SpinUnlock(BufferEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function BufferAvailable(Buffer:TBufferHandle):LongWord;
{Get the available count of buffers in an existing Buffer entry}
{Buffer: Buffer to get available count for}
{Return: Available count or INVALID_HANDLE_VALUE on error}
var
 BufferEntry:PBufferEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Buffer}
 if Buffer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 BufferEntry:=PBufferEntry(Buffer);
 if BufferEntry = nil then Exit;
 if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

 {Acquire the Lock}
 if SpinLock(BufferEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

    {Get Available Count}
    Result:=SemaphoreCount(BufferEntry.Available);
   finally
    {Release the Lock}
    SpinUnlock(BufferEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function BufferGet(Buffer:TBufferHandle):Pointer;
{Allocate an available buffer from an existing Buffer entry}
{Buffer: Handle of Buffer entry to allocate from}
{Return: A pointer to the allocated buffer or nil on error}
var
 BufferEntry:PBufferEntry;
begin
 {}
 {Check Handler}
 if Assigned(BufferGetHandler) then
  begin
   Result:=nil;

   {Check Buffer}
   if Buffer = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   BufferEntry:=PBufferEntry(Buffer);
   if BufferEntry = nil then Exit;
   if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

   {$IFDEF LOCK_DEBUG}
   if SCHEDULER_FIQ_ENABLED then
    begin
     if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(BufferDeadlockCounter);
      end;
    end
   else
    begin
     if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(BufferDeadlockCounter);
      end;
    end;
   {$ENDIF}

   {Use Handler Method}
   Result:=BufferGetHandler(BufferEntry);
  end
 else
  begin
   {Use Default Method}
   Result:=BufferGetEx(Buffer,INFINITE);
  end;
end;

{==============================================================================}

function BufferGetEx(Buffer:TBufferHandle;Timeout:LongWord):Pointer;
{Allocate an available buffer from an existing Buffer entry}
{Buffer: Handle of Buffer entry to allocate from}
{Timeout: Milliseconds to wait before timeout (0 equals do not wait, INFINITE equals wait forever)}
{Return: A pointer to the allocated buffer or nil on error}
var
 ResultCode:LongWord;
 BufferItem:PBufferItem;
 BufferEntry:PBufferEntry;
begin
 {}
 Result:=nil;

 {Check Buffer}
 if Buffer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 BufferEntry:=PBufferEntry(Buffer);
 if BufferEntry = nil then Exit;
 if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(BufferDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(BufferDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check Handler}
 if Assigned(BufferGetExHandler) then
  begin
   {Use Handler Method}
   Result:=BufferGetExHandler(BufferEntry,Timeout);
  end
 else
  begin
   {Use Default Method}
   {Check Timeout}
   if Timeout = INFINITE then
    begin
     {Wait on Buffer}
     ResultCode:=SemaphoreWait(BufferEntry.Available);
    end
   else
    begin
     {Wait on Buffer with Timeout}
     ResultCode:=SemaphoreWaitEx(BufferEntry.Available,Timeout);
    end;

   {Check Result}
   if ResultCode = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if SpinLock(BufferEntry.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Signature}
        if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

        {Get First}
        BufferItem:=BufferEntry.First;
        if BufferItem <> nil then
         begin
          {Remove First}
          BufferEntry.First:=BufferItem.Next;
          BufferItem.Next:=BufferItem;

          {Return Buffer}
          Result:=BufferItem.Buffer;
         end;
       finally
        {Release the Lock}
        SpinUnlock(BufferEntry.Lock);
       end;
      end
     else
      begin
       {Nothing}
      end;
    end
   else
    begin
     {Nothing}
    end;
  end;
end;

{==============================================================================}

function BufferFree(Buffer:Pointer):LongWord;
{Release a allocated buffer from an existing Buffer entry}
{Buffer: Pointer to the allocated buffer (As returned by BufferGet/BufferGetEx)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 BufferItem:PBufferItem;
 BufferEntry:PBufferEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Get Item}
 BufferItem:=PBufferItem(PtrUInt(Buffer) - PtrUInt(SizeOf(TBufferItem)));
 if BufferItem = nil then Exit;

 {Check Item}
 if BufferItem.Buffer <> Buffer then Exit;
 if BufferItem.Next <> BufferItem then Exit;

 {Check Buffer}
 if BufferItem.Parent = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 BufferEntry:=PBufferEntry(BufferItem.Parent);
 if BufferEntry = nil then Exit;
 if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

 {Check Handler}
 if Assigned(BufferFreeHandler) then
  begin
   {Use Handler Method}
   Result:=BufferFreeHandler(Buffer);
  end
 else
  begin
   {Use Default Method}
   {Acquire the Lock}
   if SpinLock(BufferEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

      {Add First}
      BufferItem.Next:=BufferEntry.First;
      BufferEntry.First:=BufferItem;

      {Signal Available}
      SemaphoreSignal(BufferEntry.Available);

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      SpinUnlock(BufferEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function BufferIterate(Buffer:TBufferHandle;Previous:Pointer):Pointer;
{Iterate through each of the buffers in an existing Buffer entry}
{Buffer: Handle of Buffer entry to iterate from}
{Previous: The pointer returned by the previous call or nil on first call}
{Return: A pointer to the next buffer or nil on error}

{Note: Iterate is intended to allow allocating or initializing buffers after
 a Buffer entry is created, or deallocating before a Buffer entry is destroyed.

 The function will fail if any buffers are already in use (if the count and
 available count are not equal)}
var
 BufferItem:PBufferItem;
 PreviousItem:PBufferItem;
 BufferEntry:PBufferEntry;
begin
 {}
 Result:=nil;

 {Check Buffer}
 if Buffer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 BufferEntry:=PBufferEntry(Buffer);
 if BufferEntry = nil then Exit;
 if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

 {Check Handler}
 if Assigned(BufferIterateHandler) then
  begin
   {Use Handler Method}
   Result:=BufferIterateHandler(BufferEntry,Previous);
  end
 else
  begin
   {Use Default Method}
   {Acquire the Lock}
   if SpinLock(BufferEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if BufferEntry.Signature <> BUFFER_SIGNATURE then Exit;

      {Check Available}
      if SemaphoreCount(BufferEntry.Available) = BufferEntry.Count then
       begin
        {Check Previous}
        if Previous = nil then
         begin
          {Get First}
          BufferItem:=BufferEntry.First;
          if BufferItem = nil then Exit;

          {Return Buffer}
          Result:=BufferItem.Buffer;
         end
        else
         begin
          {Get Previous}
          PreviousItem:=PBufferItem(PtrUInt(Previous) - PtrUInt(SizeOf(TBufferItem)));
          if PreviousItem = nil then Exit;
          if PreviousItem.Buffer <> Previous then Exit;
          if PreviousItem.Parent <> Buffer then Exit;

          {Get Next}
          BufferItem:=PreviousItem.Next;
          if BufferItem = nil then Exit;

          {Return Buffer}
          Result:=BufferItem.Buffer;
         end;
       end;
     finally
      {Release the Lock}
      SpinUnlock(BufferEntry.Lock);
     end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Event Functions}
function EventCreate(ManualReset,InitialState:Boolean):TEventHandle; {$IFDEF EVENT_INLINE}inline;{$ENDIF}
{Create and insert a new Event entry}
{ManualReset: Create a manual reset event if true or an auto reset event if false
              An manual reset event must be reset by calling EventReset
              An auto reset event is reset when a single waiting thread is released}
{InitialState: Set the initial state of the event to signaled if true
               or to unsignaled if false}
{Return: Handle of new Event entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 Flags:LongWord;
begin
 {}
 {Get Flags}
 Flags:=EVENT_FLAG_NONE;
 if ManualReset then Flags:=Flags or EVENT_FLAG_MANUAL_RESET;
 if InitialState then Flags:=Flags or EVENT_FLAG_INITIAL_STATE;

 {Create Event}
 Result:=EventCreateEx(Flags);
end;

{==============================================================================}

function EventCreateEx(Flags:LongWord):TEventHandle;
{Create and insert a new Event entry}
{Flags: Event flags to use for the new entry (eg EVENT_FLAG_MANUAL_RESET)}
{Return: Handle of new Event entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 EventEntry:PEventEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Create Event entry}
 if EVENT_SHARED_MEMORY then
  begin
   EventEntry:=AllocSharedMem(SizeOf(TEventEntry));
  end
 else
  begin
   EventEntry:=AllocMem(SizeOf(TEventEntry));
  end;
 if EventEntry = nil then Exit;

 {Setup Event entry}
 EventEntry.Signature:=EVENT_SIGNATURE;
 EventEntry.State:=EVENT_STATE_UNSIGNALED;
 EventEntry.Flags:=Flags;
 EventEntry.Lock:=SpinCreate;
 EventEntry.List:=INVALID_HANDLE_VALUE;
 EventEntry.Wait:=ThreadWait;
 EventEntry.WaitEx:=ThreadWaitEx;
 EventEntry.Release:=ThreadRelease;
 EventEntry.Abandon:=ThreadAbandon;

 {Setup Event entry}
 if (EventEntry.Flags and EVENT_FLAG_INITIAL_STATE) <> 0 then
  begin
   EventEntry.State:=EVENT_STATE_SIGNALED;
  end;

 {Insert Event entry}
 if SpinLock(EventTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Event entry}
    if EventTable = nil then
     begin
      EventTable:=EventEntry;
     end
    else
     begin
      EventEntry.Next:=EventTable;
      EventTable.Prev:=EventEntry;
      EventTable:=EventEntry;
     end;

    {Increment Event Count}
    Inc(EventTableCount);

    {Return Event entry}
    Result:=TEventHandle(EventEntry);
   finally
    SpinUnlock(EventTableLock);
   end;
  end
 else
  begin
   {Free Event Lock}
   SpinDestroy(EventEntry.Lock);

   {Free Event Entry}
   FreeMem(EventEntry);
  end;
end;

{==============================================================================}

function EventDestroy(Event:TEventHandle):LongWord;
{Destroy and remove an existing Event entry}
{Event: Handle of Event entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 EventEntry:PEventEntry;
 PrevEntry:PEventEntry;
 NextEntry:PEventEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Event}
 if Event = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 EventEntry:=PEventEntry(Event);
 if EventEntry = nil then Exit;
 if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

 {Remove Event entry}
 if SpinLock(EventTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=SpinLock(EventEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if EventEntry.Signature <> EVENT_SIGNATURE then
     begin
      {Release the Lock}
      Result:=SpinUnlock(EventEntry.Lock);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Event entry}
    EventEntry.Signature:=0;

    {Check Waiting Threads}
    while ListNotEmpty(EventEntry.List) do
     begin
      {Abandon waiting thread}
      EventEntry.Abandon(EventEntry.List);
     end;

    {Unlink Event entry}
    PrevEntry:=EventEntry.Prev;
    NextEntry:=EventEntry.Next;
    if PrevEntry = nil then
     begin
      EventTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Event Count}
    Dec(EventTableCount);

    {Release the Lock}
    Result:=SpinUnlock(EventEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Free Event List}
    if EventEntry.List <> INVALID_HANDLE_VALUE then
     begin
      ListDestroy(EventEntry.List);
     end;

    {Free Event Lock}
    SpinDestroy(EventEntry.Lock);

    {Free Event Entry}
    FreeMem(EventEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(EventTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function EventState(Event:TEventHandle):LongWord;
{Get the current state of an existing Event entry}
{Event: Event to get state for}
{Return: Current state or INVALID_HANDLE_VALUE on error}
var
 EventEntry:PEventEntry;
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Event}
 if Event = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 EventEntry:=PEventEntry(Event);
 if EventEntry = nil then Exit;
 if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

 {Acquire the Lock}
 if SpinLock(EventEntry.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

    {Get State}
    Result:=EventEntry.State;
   finally
    {Release the Lock}
    SpinUnlock(EventEntry.Lock);
   end;
  end;
end;

{==============================================================================}

function EventWait(Event:TEventHandle):LongWord;
{Wait on an existing Event entry

 If the Event is currently signaled then simply return immediately

 If the Event is currently unsignaled then wait for it to be signaled
 before returning}
{Event: Event to wait on}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 EventEntry:PEventEntry;
begin
 {}
 {Check the Handler}
 if Assigned(EventWaitHandler) then
  begin
   Result:=ERROR_INVALID_PARAMETER;

   {Check Event}
   if Event = INVALID_HANDLE_VALUE then Exit;

   {Check the Handle}
   EventEntry:=PEventEntry(Event);
   if EventEntry = nil then Exit;
   if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

   {$IFDEF LOCK_DEBUG}
   if SCHEDULER_FIQ_ENABLED then
    begin
     if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(EventDeadlockCounter);
      end;
    end
   else
    begin
     if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
      begin
       Inc(EventDeadlockCounter);
      end;
    end;
   {$ENDIF}

   {Use the Handler method}
   Result:=EventWaitHandler(EventEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=EventWaitEx(Event,INFINITE);
  end;
end;

{==============================================================================}

function EventWaitEx(Event:TEventHandle;Timeout:LongWord):LongWord;
{Wait on an existing Event entry

 If the Event is currently signaled then simply return immediately

 If the Event is currently unsignaled then wait for it to be signaled
 before returning}
{Event: Event to wait on}
{Timeout: Time in milliseconds to wait for the event to be signaled
          0 = No Wait
          INFINITE = Wait Indefinitely}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 WaitResult:LongWord;
 EventEntry:PEventEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Event}
 if Event = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 EventEntry:=PEventEntry(Event);
 if EventEntry = nil then Exit;
 if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(EventWaitExHandler) then
  begin
   {Use the Handler method}
   Result:=EventWaitExHandler(EventEntry,Timeout);
  end
 else
  begin
   {Use the Default method}
   {Lock the Event}
   if SpinLock(EventEntry.Lock) = ERROR_SUCCESS then
    begin
     Unlock:=True;
     try
      {Check Signature}
      if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

      {Check Timeout}
      if Timeout = 0 then
       begin
        {Check State}
        Result:=ERROR_WAIT_TIMEOUT;
        if EventEntry.State <> EVENT_STATE_SIGNALED then Exit;
       end;

      {Check State}
      if EventEntry.State = EVENT_STATE_SIGNALED then
       begin
        {Check Flags}
        if (EventEntry.Flags and EVENT_FLAG_MANUAL_RESET) = 0 then
         begin
          {Auto Reset}
          {Reset State}
          EventEntry.State:=EVENT_STATE_UNSIGNALED;
         end;
       end
      else
       begin
        {Check List}
        if EventEntry.List = INVALID_HANDLE_VALUE then
         begin
          {Create List}
          EventEntry.List:=ListCreateEx(LIST_TYPE_WAIT_EVENT,SchedulerGetListFlags(LIST_TYPE_WAIT_EVENT));
         end;

        {Check Timeout}
        if Timeout = INFINITE then
         begin
          {Wait on Event}
          EventEntry.Wait(EventEntry.List,EventEntry.Lock,LOCK_FLAG_NONE);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end
        else
         begin
          {Wait on Event with Timeout}
          EventEntry.WaitEx(EventEntry.List,EventEntry.Lock,LOCK_FLAG_NONE,Timeout);
          Unlock:=False;

          {Check Result}
          WaitResult:=ThreadGetWaitResult;
          if WaitResult = WAIT_TIMEOUT then
           begin
            Result:=ERROR_WAIT_TIMEOUT;
            Exit;
           end
          else if WaitResult = WAIT_ABANDONED then
           begin
            Result:=ERROR_WAIT_ABANDONED;
            Exit;
           end
          else if WaitResult <> ERROR_SUCCESS then
           begin
            Result:=WaitResult;
            Exit;
           end;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Event}
      if Unlock then SpinUnlock(EventEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function EventSet(Event:TEventHandle):LongWord;
{Set (Signal) an existing Event entry

 If the event is currently signaled then return with no action

 If the event is unsignaled then, if the event is manual reset release
 all waiting threads and return. If the event is auto reset release one
 waiting thread, unsignal the event and return

 If no threads are waiting then simply signal the event and return, if
 the event is auto reset then the next thread to wait will unsignal the
 event}
{Event: Event to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 EventEntry:PEventEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Event}
 if Event = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 EventEntry:=PEventEntry(Event);
 if EventEntry = nil then Exit;
 if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(EventSetHandler) then
  begin
   {Use the Handler method}
   Result:=EventSetHandler(EventEntry);
  end
 else
  begin
   {Use the Default method}
   {Lock the Event}
   if SpinLock(EventEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

      {Check State}
      if EventEntry.State = EVENT_STATE_UNSIGNALED then
       begin
        {Set State}
        EventEntry.State:=EVENT_STATE_SIGNALED;

        {Check Flags}
        if (EventEntry.Flags and EVENT_FLAG_MANUAL_RESET) = 0 then
         begin
          {Auto Reset}
          {Check List}
          while ListNotEmpty(EventEntry.List) do
           begin
            {Release one thread waiting on Event}
            if EventEntry.Release(EventEntry.List) = ERROR_SUCCESS then
             begin
              {Reset State}
              EventEntry.State:=EVENT_STATE_UNSIGNALED;

              Break;
             end;
           end;
         end
        else
         begin
          {Manual Reset}
          {Check List}
          while ListNotEmpty(EventEntry.List) do
           begin
            {Release all threads waiting on Event}
            EventEntry.Release(EventEntry.List);
           end;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Event}
      SpinUnlock(EventEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function EventReset(Event:TEventHandle):LongWord;
{Reset (Unsignal) an existing Event entry

 If the event is currently unsignaled then return with no action

 If the event is signaled then unsignal the event and return}
{Event: Event to reset}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 EventEntry:PEventEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Event}
 if Event = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 EventEntry:=PEventEntry(Event);
 if EventEntry = nil then Exit;
 if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(EventResetHandler) then
  begin
   {Use the Handler method}
   Result:=EventResetHandler(EventEntry);
  end
 else
  begin
   {Use the Default method}
   {Lock the Event}
   if SpinLock(EventEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

      {Check State}
      if EventEntry.State = EVENT_STATE_SIGNALED then
       begin
        {Reset State}
        EventEntry.State:=EVENT_STATE_UNSIGNALED;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Event}
      SpinUnlock(EventEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function EventPulse(Event:TEventHandle):LongWord;
{Pulse (Set then Reset) an existing Event entry

 If the event is currently signaled then unsignal the event and return

 If the event is unsignaled then, if the event is manual reset release
 all waiting threads, unsignal the event and return. If the event is
 auto reset release one waiting thread, unsignal the event and return

 If no threads are waiting then simply unsignal the event and return}
{Event: Event to pulse}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 EventEntry:PEventEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Event}
 if Event = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 EventEntry:=PEventEntry(Event);
 if EventEntry = nil then Exit;
 if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

 {$IFDEF LOCK_DEBUG}
 if SCHEDULER_FIQ_ENABLED then
  begin
   if not(GetFIQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end
 else
  begin
   if not(GetIRQ) and (InitializationCompleted[CPUGetCurrent]) then
    begin
     Inc(EventDeadlockCounter);
    end;
  end;
 {$ENDIF}

 {Check the Handler}
 if Assigned(EventPulseHandler) then
  begin
   {Use the Handler method}
   Result:=EventPulseHandler(EventEntry);
  end
 else
  begin
   {Use the Default method}
   {Lock the Event}
   if SpinLock(EventEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if EventEntry.Signature <> EVENT_SIGNATURE then Exit;

      {Check State}
      if EventEntry.State = EVENT_STATE_SIGNALED then
       begin
        {Reset State}
        EventEntry.State:=EVENT_STATE_UNSIGNALED;
       end
      else
       begin
        {Set State}
        EventEntry.State:=EVENT_STATE_SIGNALED;

        {Check Flags}
        if (EventEntry.Flags and EVENT_FLAG_MANUAL_RESET) = 0 then
         begin
          {Auto Reset}
          {Check List}
          while ListNotEmpty(EventEntry.List) do
           begin
            {Release one thread waiting on Event}
            if EventEntry.Release(EventEntry.List) = ERROR_SUCCESS then
             begin
              Break;
             end;
           end;
         end
        else
         begin
          {Manual Reset}
          {Check List}
          while ListNotEmpty(EventEntry.List) do
           begin
            {Release all threads waiting on Event}
            EventEntry.Release(EventEntry.List);
           end;
         end;

        {Reset State}
        EventEntry.State:=EVENT_STATE_UNSIGNALED;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Unlock the Event}
      SpinUnlock(EventEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Timer Functions}
function TimerCreate(Interval:LongWord;Enabled,Reschedule:Boolean;Event:TTimerEvent;Data:Pointer):TTimerHandle; {$IFDEF TIMER_INLINE}inline;{$ENDIF}
{Create and insert a new Timer entry}
{Interval: Number of milliseconds between timer events}
{Enabled: If true then timer generates events}
{Reschedule: If true then reschedule timer after each event}
{Event: The function to call when the timer event is generated}
{Data: Data to be passed to the function when the timer event is generated (May be nil)}
{Return: Handle of new Timer entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 State:LongWord;
 Flags:LongWord;
begin
 {}
 {Get State}
 State:=TIMER_STATE_DISABLED;
 if Enabled then State:=TIMER_STATE_ENABLED;

 {Get Flags}
 Flags:=TIMER_FLAG_NONE;
 if Reschedule then Flags:=TIMER_FLAG_RESCHEDULE;

 {Call TimerCreateEx}
 Result:=TimerCreateEx(Interval,State,Flags,Event,Data);
end;

{==============================================================================}

function TimerCreateEx(Interval,State,Flags:LongWord;Event:TTimerEvent;Data:Pointer):TTimerHandle;
{Create and insert a new Timer entry}
{Interval: Number of milliseconds between timer events}
{State: State of timer entry (eg TIMER_STATE_ENABLED)}
{Flags: Flags of timer entry (eg TIMER_FLAG_RESCHEDULE)}
{Event: The function to call when the timer event is generated}
{Data: Data to be passed to the function when the timer event is generated (May be nil)}
{Return: Handle of new Timer entry or INVALID_HANDLE_VALUE if entry could not be created}
var
 Size:LongWord;
 Ticks:Integer;
 TimerEntry:PTimerEntry;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Interval}
 if Interval < 1 then Exit;

 {Check State}
 if (State <> TIMER_STATE_DISABLED) and (State <> TIMER_STATE_ENABLED) then Exit;

 {Check Event}
 if not(Assigned(Event)) then Exit;

 {Get Size}
 Size:=SizeOf(TTimerEntry);
 if (Flags and TIMER_FLAG_WORKER) <> 0 then Size:=SizeOf(TTimerEntry) + SizeOf(TWorkerRequest);

 {Create Timer entry}
 TimerEntry:=AllocMem(Size);
 if TimerEntry = nil then Exit;

 {Setup Timer entry}
 TimerEntry.Signature:=TIMER_SIGNATURE;
 TimerEntry.Interval:=Interval;
 TimerEntry.State:=State;
 TimerEntry.Flags:=Flags;
 TimerEntry.Lock:=SpinCreate;
 TimerEntry.Event:=Event;
 TimerEntry.Data:=Data;
 TimerEntry.TimerList:=nil;
 TimerEntry.TimerItem.Timer:=TTimerHandle(TimerEntry);

 {Check Flags}
 if (Flags and TIMER_FLAG_WORKER) <> 0 then
  begin
   {Get Worker request}
   WorkerRequest:=PWorkerRequest(PtrUInt(TimerEntry) + PtrUInt(SizeOf(TTimerEntry)));

   {Setup Worker request}
   WorkerRequest.Signature:=WORKER_SIGNATURE;
   WorkerRequest.Interval:=0;
   WorkerRequest.Flags:=WORKER_FLAG_NOFREE;
   WorkerRequest.Lock:=INVALID_HANDLE_VALUE;
   WorkerRequest.Timer:=INVALID_HANDLE_VALUE;
   WorkerRequest.Task:=TWorkerTask(Event);
   WorkerRequest.Data:=Data;
   WorkerRequest.Callback:=nil;

   {Check Priority}
   if (Flags and TIMER_FLAG_PRIORITY) <> 0 then
    begin
     WorkerRequest.Flags:=WorkerRequest.Flags or WORKER_FLAG_PRIORITY;
    end;
  end;

 {Insert Timer entry}
 if SpinLock(TimerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Timer entry}
    if TimerTable = nil then
     begin
      TimerTable:=TimerEntry;
     end
    else
     begin
      TimerEntry.Next:=TimerTable;
      TimerTable.Prev:=TimerEntry;
      TimerTable:=TimerEntry;
     end;

    {Increment Timer Count}
    Inc(TimerTableCount);

    {Return Timer entry}
    Result:=TTimerHandle(TimerEntry);

    {Check State (No need to lock)}
    if TimerEntry.State = TIMER_STATE_ENABLED then
     begin
      {Check Immediate}
      if (TimerEntry.Flags and TIMER_FLAG_IMMEDIATE) <> 0 then
       begin
        {Calculate Ticks}
        Ticks:=CLOCK_TICKS_PER_MILLISECOND; {1 millisecond before first event}
       end
      else
       begin
        {Calculate Ticks}
        Ticks:=CLOCK_TICKS_PER_MILLISECOND * TimerEntry.Interval;
       end;

      {Insert in List}
      TimerInsertKey(Result,Ticks);
     end;
   finally
    SpinUnlock(TimerTableLock);
   end;
  end
 else
  begin
   {Free Timer Lock}
   SpinDestroy(TimerEntry.Lock);

   {Free Timer Entry}
   FreeMem(TimerEntry);
  end;
end;

{==============================================================================}

function TimerDestroy(Timer:TTimerHandle):LongWord;
{Destroy and remove an existing Timer entry}
{Timer: Handle of Timer entry to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 TimerEntry:PTimerEntry;
 PrevEntry:PTimerEntry;
 NextEntry:PTimerEntry;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timer}
 if Timer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 TimerEntry:=PTimerEntry(Timer);
 if TimerEntry = nil then Exit;
 if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

 {Disable the Timer}
 Result:=TimerDisable(Timer);
 if Result <> ERROR_SUCCESS then Exit;

 {Remove Timer entry}
 if SpinLock(TimerTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    Result:=SpinLock(TimerEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Signature}
    if TimerEntry.Signature <> TIMER_SIGNATURE then
     begin
      {Release the Lock}
      Result:=SpinUnlock(TimerEntry.Lock);
      if Result <> ERROR_SUCCESS then Exit;

      {Return Result}
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Invalidate Timer entry}
    TimerEntry.Signature:=0;

    {Check Flags}
    if (TimerEntry.Flags and TIMER_FLAG_WORKER) <> 0 then
     begin
      {Get Worker request}
      WorkerRequest:=PWorkerRequest(PtrUInt(TimerEntry) + PtrUInt(SizeOf(TTimerEntry)));

      {Invalidate Worker request}
      WorkerRequest.Signature:=0;
     end;

    {Unlink Timer entry}
    PrevEntry:=TimerEntry.Prev;
    NextEntry:=TimerEntry.Next;
    if PrevEntry = nil then
     begin
      TimerTable:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=nil;
       end;
     end
    else
     begin
      PrevEntry.Next:=NextEntry;
      if NextEntry <> nil then
       begin
        NextEntry.Prev:=PrevEntry;
       end;
     end;

    {Decrement Timer Count}
    Dec(TimerTableCount);

    {Release the Lock}
    Result:=SpinUnlock(TimerEntry.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Free Timer Lock}
    SpinDestroy(TimerEntry.Lock);

    {Free Timer Entry}
    FreeMem(TimerEntry);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    SpinUnlock(TimerTableLock);
   end;
  end
 else
  begin
   {Return Result}
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimerEnable(Timer:TTimerHandle):LongWord;
{Enable an existing Timer entry (Timer events will be generated)}
{Timer: Handle of Timer entry to enable}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 TimerEntry:PTimerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timer}
 if Timer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 TimerEntry:=PTimerEntry(Timer);
 if TimerEntry = nil then Exit;
 if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(TimerEnableHandler) then
  begin
   {Use the Handler method}
   Result:=TimerEnableHandler(TimerEntry);
  end
 else
  begin
   {Use the Default method}
   Result:=TimerEnableEx(Timer,TimerEntry.Interval,TimerEntry.Event,TimerEntry.Data);
  end;
end;

{==============================================================================}

function TimerEnableEx(Timer:TTimerHandle;Interval:LongWord;Event:TTimerEvent;Data:Pointer):LongWord;
{Enable and update an existing Timer entry (Timer events will be generated)}
{Timer: Handle of Timer entry to enable}
{Interval: Number of milliseconds between timer events}
{Event: The function to call when the timer event is generated}
{Data: Data to be passed to the function when the timer event is generated (May be nil)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Ticks:Integer;
 TimerEntry:PTimerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timer}
 if Timer = INVALID_HANDLE_VALUE then Exit;

 {Check Interval}
 if Interval < 1 then Exit;

 {Check Event}
 if not(Assigned(Event)) then Exit;

 {Check the Handle}
 TimerEntry:=PTimerEntry(Timer);
 if TimerEntry = nil then Exit;
 if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(TimerEnableExHandler) then
  begin
   {Use the Handler method}
   Result:=TimerEnableExHandler(TimerEntry,Interval,Event,Data);
  end
 else
  begin
   {Use the Default method}
   {Acquire the Lock}
   if SpinLock(TimerEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

      {Check State}
      if TimerEntry.State = TIMER_STATE_DISABLED then
       begin
        {Set Interval}
        TimerEntry.Interval:=Interval;

        {Set State}
        TimerEntry.State:=TIMER_STATE_ENABLED;

        {Set Event}
        TimerEntry.Event:=Event;

        {Set Data}
        TimerEntry.Data:=Data;

        {Calculate Ticks}
        Ticks:=CLOCK_TICKS_PER_MILLISECOND * TimerEntry.Interval;

        {Insert in List}
        TimerInsertKey(Timer,Ticks);
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      SpinUnlock(TimerEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function TimerDisable(Timer:TTimerHandle):LongWord;
{Disable an existing Timer entry (Timer events will not be generated)}
{Timer: Handle of Timer entry to disable}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 TimerEntry:PTimerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timer}
 if Timer = INVALID_HANDLE_VALUE then Exit;

 {Check the Handle}
 TimerEntry:=PTimerEntry(Timer);
 if TimerEntry = nil then Exit;
 if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

 {Check the Handler}
 if Assigned(TimerDisableHandler) then
  begin
   {Use the Handler method}
   Result:=TimerDisableHandler(TimerEntry);
  end
 else
  begin
   {Use the Default method}
   {Acquire the Lock}
   if SpinLock(TimerEntry.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Signature}
      if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

      {Check State}
      if TimerEntry.State = TIMER_STATE_ENABLED then
       begin
        {Set State}
        TimerEntry.State:=TIMER_STATE_DISABLED;

        {Remove from List}
        TimerDeleteKey(Timer);
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      SpinUnlock(TimerEntry.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function TimerDequeue:TTimerHandle;
{Get and remove the first timer from the Timer list}
{Return: Handle of dequeued Timer or INVALID_HANDLE_VALUE on failure}
begin
 {}
 Result:=TimerDequeueEx(True,True);
end;

{==============================================================================}

function TimerDequeueEx(Lock,Unlock:Boolean):TTimerHandle;
{Get and remove the first timer from the Timer list}
{Lock: Lock the Timer list on entry if True}
{Unlock: Unlock the Timer list on exit if True}
{Return: Handle of dequeued Timer or INVALID_HANDLE_VALUE on failure}
{Note: Extended version used internally by the timer trigger}
var
 ResultCode:LongWord;
 TimerItem:PTimerItem;
 TimerEntry:PTimerEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check the List}
 if TimerList = nil then Exit;

 {Lock the List}
 if Lock then
  begin
   if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQFIQ(TimerList.Lock);
    end
   else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
    begin
     ResultCode:=SpinLockFIQ(TimerList.Lock);
    end
   else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQ(TimerList.Lock);
    end
   else
    begin
     ResultCode:=SpinLock(TimerList.Lock);
    end;
  end
 else
  begin
   ResultCode:=ERROR_SUCCESS;
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Get Item (First)}
    TimerItem:=TimerList.First;
    if TimerItem <> nil then
     begin
      {Remove First}
      TimerList.First:=TimerItem.Next;
      {Check Next}
      if TimerItem.Next = nil then
       begin
        TimerList.Last:=nil;
       end
      else
       begin
        TimerItem.Next.Prev:=nil;
       end;

      {Decrement Count}
      Dec(TimerList.Count);

      {Return Result}
      Result:=TimerItem.Timer;

      {Release Item}
      {Check Timer}
      if Result = INVALID_HANDLE_VALUE then Exit;

      {Check the Handle}
      TimerEntry:=PTimerEntry(Result);
      if TimerEntry = nil then Exit;
      if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

      {Set TimerList}
      TimerEntry.TimerList:=nil;
     end;
   finally
    {Unlock the List}
    if Unlock then
     begin
      if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
       begin
        SpinUnlockIRQFIQ(TimerList.Lock);
       end
      else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
       begin
        SpinUnlockFIQ(TimerList.Lock);
       end
      else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
       begin
        SpinUnlockIRQ(TimerList.Lock);
       end
      else
       begin
        SpinUnlock(TimerList.Lock);
       end;
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function TimerFirstKey:Integer;
{Get the first Key value from the Timer list}
{Return: First Key value from timer list or TIMER_KEY_NONE on failure}
begin
 {}
 Result:=TimerFirstKeyEx(True,True);
end;

{==============================================================================}

function TimerFirstKeyEx(Lock,Unlock:Boolean):Integer;
{Get the first Key value from the Timer list}
{Lock: Lock the Timer list on entry if True}
{Unlock: Unlock the Timer list on exit if True}
{Return: First Key value from timer list or TIMER_KEY_NONE on failure}
{Note: Extended version used internally by the timer trigger}
var
 ResultCode:LongWord;
 TimerItem:PTimerItem;
begin
 {}
 Result:=TIMER_KEY_NONE;

 {Check the List}
 if TimerList = nil then Exit;

 {Lock the List}
 if Lock then
  begin
   if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQFIQ(TimerList.Lock);
    end
   else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
    begin
     ResultCode:=SpinLockFIQ(TimerList.Lock);
    end
   else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
    begin
     ResultCode:=SpinLockIRQ(TimerList.Lock);
    end
   else
    begin
     ResultCode:=SpinLock(TimerList.Lock);
    end;
  end
 else
  begin
   ResultCode:=ERROR_SUCCESS;
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Get Item (First)}
    TimerItem:=TimerList.First;
    if TimerItem <> nil then
     begin
      {Return Key}
      Result:=TimerItem.Key;
     end;
   finally
    {Unlock the List}
    if Unlock then
     begin
      if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
       begin
        SpinUnlockIRQFIQ(TimerList.Lock);
       end
      else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
       begin
        SpinUnlockFIQ(TimerList.Lock);
       end
      else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
       begin
        SpinUnlockIRQ(TimerList.Lock);
       end
      else
       begin
        SpinUnlock(TimerList.Lock);
       end;
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function TimerInsertKey(Timer:TTimerHandle;Key:Integer):LongWord;
{Insert the supplied timer in the Timer list in delta ascending order based on Key}
{Timer: Handle of timer to be inserted}
{Key: The key to order the insertion on}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Caller must hold the lock on the timer}
var
 Offset:Integer;
 ResultCode:LongWord;
 TimerItem:PTimerItem;
 PrevItem:PTimerItem;
 NextItem:PTimerItem;
 TimerEntry:PTimerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timer}
 if Timer = INVALID_HANDLE_VALUE then Exit;

 {Check the List}
 if TimerList = nil then Exit;

 {Lock the List}
 if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
  begin
   ResultCode:=SpinLockFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQ(TimerList.Lock);
  end
 else
  begin
   ResultCode:=SpinLock(TimerList.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Get Item}
    {Check the Handle}
    TimerEntry:=PTimerEntry(Timer);
    if TimerEntry = nil then Exit;
    if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

    {Use Timer Item}
    TimerItem:=@TimerEntry.TimerItem;
    TimerItem.Key:=0;

    {Set TimerList}
    TimerEntry.TimerList:=TimerList;

    {Find Item}
    Offset:=0;
    PrevItem:=nil;
    NextItem:=TimerList.First;
    while NextItem <> nil do
     begin
      {Delta Ascending}
      if Key < (Offset + NextItem.Key) then
       begin
        Dec(NextItem.Key,(Key - Offset));
        Break;
       end;
      Inc(Offset,NextItem.Key);
      PrevItem:=NextItem;
      NextItem:=NextItem.Next;
     end;

    {Insert Item}
    TimerItem.Key:=(Key - Offset);
    {Get Prev/Next}
    TimerItem.Prev:=PrevItem;
    TimerItem.Next:=NextItem;
    {Check Prev}
    if PrevItem = nil then
     begin
      TimerList.First:=TimerItem;
      {Check Next}
      if NextItem = nil then
       begin
        TimerList.Last:=TimerItem;
       end
      else
       begin
        NextItem.Prev:=TimerItem;
       end;
     end
    else
     begin
      PrevItem.Next:=TimerItem;
      {Check Next}
      if NextItem = nil then
       begin
        TimerList.Last:=TimerItem;
       end
      else
       begin
        NextItem.Prev:=TimerItem;
       end;
     end;

    {Increment Count}
    Inc(TimerList.Count);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock the List}
    if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
     begin
      SpinUnlockIRQFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
     begin
      SpinUnlockFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
     begin
      SpinUnlockIRQ(TimerList.Lock);
     end
    else
     begin
      SpinUnlock(TimerList.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimerDeleteKey(Timer:TTimerHandle):LongWord;
{Delete the supplied timer from the Timer list}
{Timer: Handle of timer to be deleted}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Caller must hold the lock on the timer}
var
 ResultCode:LongWord;
 TimerItem:PTimerItem;
 PrevItem:PTimerItem;
 NextItem:PTimerItem;
 TimerEntry:PTimerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Timer}
 if Timer = INVALID_HANDLE_VALUE then Exit;

 {Check the List}
 if TimerList = nil then Exit;

 {Lock the List}
 if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
  begin
   ResultCode:=SpinLockFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQ(TimerList.Lock);
  end
 else
  begin
   ResultCode:=SpinLock(TimerList.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Find Item}
    TimerItem:=TimerList.First;
    while TimerItem <> nil do
     begin
      if TimerItem.Timer = Timer then
       begin
        Break;
       end;
      TimerItem:=TimerItem.Next;
     end;

    {Check Element}
    if TimerItem <> nil then
     begin
      {Delete Element}
      {Get Prev/Next}
      PrevItem:=TimerItem.Prev;
      NextItem:=TimerItem.Next;
      {Check Prev}
      if PrevItem = nil then
       begin
        TimerList.First:=NextItem;
        {Check Next}
        if NextItem = nil then
         begin
          TimerList.Last:=nil;
         end
        else
         begin
          NextItem.Prev:=nil;
         end;
       end
      else
       begin
        PrevItem.Next:=NextItem;
        {Check Next}
        if NextItem = nil then
         begin
          TimerList.Last:=PrevItem;
         end
        else
         begin
          NextItem.Prev:=PrevItem;
         end;
       end;

      {Check Next}
      if NextItem <> nil then
       begin
        {Update Key}
        Inc(NextItem.Key,TimerItem.Key);
       end;

      {Decrement Count}
      Dec(TimerList.Count);

      {Release Element}
      {Check the Handle}
      TimerEntry:=PTimerEntry(Timer);
      if TimerEntry = nil then Exit;
      if TimerEntry.Signature <> TIMER_SIGNATURE then Exit;

      {Set TimerList}
      TimerEntry.TimerList:=nil;

      {Return Result}
      Result:=ERROR_SUCCESS;
     end;
   finally
    {Unlock the List}
    if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
     begin
      SpinUnlockIRQFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
     begin
      SpinUnlockFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
     begin
      SpinUnlockIRQ(TimerList.Lock);
     end
    else
     begin
      SpinUnlock(TimerList.Lock);
     end;
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TimerDecrementKey:Integer;
{Decrement the first Key value in the Timer list}
{Return: First Key value in timer list after decrement or TIMER_KEY_NONE on failure}
var
 ResultCode:LongWord;
 TimerItem:PTimerItem;
begin
 {}
 Result:=TIMER_KEY_NONE;

 {Check the List}
 if TimerList = nil then Exit;

 {Lock the List}
 if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
  begin
   ResultCode:=SpinLockFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQ(TimerList.Lock);
  end
 else
  begin
   ResultCode:=SpinLock(TimerList.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Get First Item}
    TimerItem:=TimerList.First;
    if TimerItem <> nil then
     begin
      {Decrement Key}
      if TimerItem.Key > TIMER_KEY_MIN then
       begin
        Dec(TimerItem.Key);
       end;

      {Return Result}
      Result:=TimerItem.Key;
     end;
   finally
    {Unlock the List}
    if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
     begin
      SpinUnlockIRQFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
     begin
      SpinUnlockFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
     begin
      SpinUnlockIRQ(TimerList.Lock);
     end
    else
     begin
      SpinUnlock(TimerList.Lock);
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function TimerIsEmpty:Boolean;
{Check if the Timer list is empty}
{Return: True if Timer list is empty or does not exist, False if Timer list is not empty}
var
 ResultCode:LongWord;
begin
 {}
 Result:=True; {Default to True}

 {Check the List}
 if TimerList = nil then Exit;

 {Lock the List}
 if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
  begin
   ResultCode:=SpinLockFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQ(TimerList.Lock);
  end
 else
  begin
   ResultCode:=SpinLock(TimerList.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Empty}
    if TimerList.First = nil then Exit;

    Result:=False;
   finally
    {Unlock the List}
    if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
     begin
      SpinUnlockIRQFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
     begin
      SpinUnlockFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
     begin
      SpinUnlockIRQ(TimerList.Lock);
     end
    else
     begin
      SpinUnlock(TimerList.Lock);
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function TimerNotEmpty:Boolean;
{Check if the Timer list is not empty}
{Return: True if Timer list is not empty, False if Timer list is empty or does not exist}
var
 ResultCode:LongWord;
begin
 {}
 Result:=False;

 {Check the List}
 if TimerList = nil then Exit;

 {Lock the List}
 if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
  begin
   ResultCode:=SpinLockFIQ(TimerList.Lock);
  end
 else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
  begin
   ResultCode:=SpinLockIRQ(TimerList.Lock);
  end
 else
  begin
   ResultCode:=SpinLock(TimerList.Lock);
  end;
 if ResultCode = ERROR_SUCCESS then
  begin
   try
    {Check Empty}
    if TimerList.First = nil then Exit;

    Result:=True;
   finally
    {Unlock the List}
    if (TimerList.Flags and LOCK_FLAG_IRQFIQ) <> 0 then
     begin
      SpinUnlockIRQFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_FIQ) <> 0 then
     begin
      SpinUnlockFIQ(TimerList.Lock);
     end
    else if (TimerList.Flags and LOCK_FLAG_IRQ) <> 0 then
     begin
      SpinUnlockIRQ(TimerList.Lock);
     end
    else
     begin
      SpinUnlock(TimerList.Lock);
     end;
   end;
  end
 else
  begin
   {Nothing}
  end;
end;

{==============================================================================}

function TimerCheck:LongWord;
{Check if the timer list is empty, if not then decrement the first key

 If the key reaches zero, return success to indicate there are timers to
 be triggered}
{Return: ERROR_SUCCESS if the first key is zero, ERROR_NO_MORE_ITEMS if list is empty or another error code on failure}
{Note: Called by clock interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Handler}
 if Assigned(TimerCheckHandler) then
  begin
   {Use the Handler method}
   Result:=TimerCheckHandler;
  end
 else
  begin
   {Use the Default method}
   {Setup Result}
   Result:=ERROR_NO_MORE_ITEMS;

   {Check Timer List}
   if TimerDecrementKey <= 0 then
    begin
     Result:=ERROR_SUCCESS;
    end;
  end;
end;

{==============================================================================}

function TimerTrigger:LongWord;
{Remove all entries from the timer list that have reached their interval

 For each timer a message will be sent to the Timer thread to call the event}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Called by clock interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
var
 Message:TMessage;
 Timer:TTimerHandle;
 TimerEntry:PTimerEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Handler}
 if Assigned(TimerTriggerHandler) then
  begin
   {Use the Handler method}
   Result:=TimerTriggerHandler;
  end
 else
  begin
   {Use the Default method}
   {Trigger Timers}
   FillChar(Message,SizeOf(TMessage),0);
   while TimerFirstKeyEx(True,False) <= 0 do
    begin
     {Dequeue Timer}
     Timer:=TimerDequeueEx(False,True);
     if Timer <> INVALID_HANDLE_VALUE then
      begin
       {Check the Timer}
       TimerEntry:=PTimerEntry(Timer);
       if (TimerEntry <> nil) and (TimerEntry.Signature = TIMER_SIGNATURE) then
        begin
         Message.Msg:=PtrUInt(TimerEntry);

         {Check the Flags}
         if (TimerEntry.Flags and TIMER_FLAG_PRIORITY) = 0 then
          begin
           MessageslotSend(TimerMessageslot,Message);
          end
         else
          begin
           MessageslotSend(TimerPriorityMessageslot,Message);
          end;
        end;
      end;
    end;
   {Unlock List}
   TimerFirstKeyEx(False,True);

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}
{==============================================================================}
{Worker Functions}
function WorkerSchedule(Interval:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
{Schedule a task to be performed by a worker thread now or in the future}
{Interval: The number of milliseconds before the task is to be performed (0 for immediate)}
{Task: The function to be called by the worker when the interval has elapsed}
{Data: A pointer to user defined data which will be passed to the task function (Optional)}
{Callback: The function to be called by the worker when the task has completed (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Task}
 if not Assigned(Task) then Exit;

 {Check Data}
 {if Data = nil then Exit;} {May be nil}

 {Check Callback}
 {if not Assigned(Callback) then Exit;}  {May be nil}

 {Schedule Worker}
 Result:=LongWord(WorkerScheduleEx(Interval,WORKER_FLAG_NONE,Task,Data,Callback));
end;

{==============================================================================}

function WorkerScheduleEx(Interval,Flags:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):TWorkerHandle;
{Schedule a task to be performed by a worker thread now or in the future}
{Interval: The number of milliseconds before the task is to be performed (0 for immediate)}
{Flags: The flags for the task (eg WORKER_FLAG_RESCHEDULE)}
{Task: The function to be called by the worker when the interval has elapsed}
{Data: A pointer to user defined data which will be passed to the task function (Optional)}
{Callback: The function to be called by the worker when the task has completed (Optional)}
{Return: Handle of new Worker task or INVALID_HANDLE_VALUE if task could not be created}
{Note: If the flags do not contain WORKER_FLAG_RESCHEDULE then return will be ERROR_SUCCESS}
var
 Size:LongWord;
 Message:TMessage;
 WorkerRequest:PWorkerRequest;
 InitialRequest:PWorkerRequest;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Task}
 if not Assigned(Task) then Exit;

 {Check Data}
 {if Data = nil then Exit;} {May be nil}

 {Check Callback}
 {if not Assigned(Callback) then Exit;}  {May be nil}

 {Check Flags}
 if (Flags and WORKER_FLAG_INTERNAL) <> 0 then Exit;

 {Get Size}
 Size:=SizeOf(TWorkerRequest);
 if ((Flags and WORKER_FLAG_RESCHEDULE) <> 0) and (Interval > 0) then Size:=SizeOf(TWorkerRequest) + SizeOf(TWorkerRequest);

 {Create Worker Request}
 WorkerRequest:=AllocMem(Size);
 if WorkerRequest = nil then Exit;

 {Setup Worker Request}
 WorkerRequest.Signature:=WORKER_SIGNATURE;
 WorkerRequest.Interval:=Interval;
 WorkerRequest.Flags:=Flags;
 WorkerRequest.Lock:=INVALID_HANDLE_VALUE;
 WorkerRequest.Timer:=INVALID_HANDLE_VALUE;
 WorkerRequest.Task:=Task;
 WorkerRequest.Data:=Data;
 WorkerRequest.Callback:=Callback;

 {Check Flags}
 if ((Flags and WORKER_FLAG_RESCHEDULE) <> 0) and (Interval > 0) then
  begin
   {Get Initial/Repeat Request}
   InitialRequest:=PWorkerRequest(PtrUInt(WorkerRequest) + PtrUInt(SizeOf(TWorkerRequest)));

   {Setup Initial/Repeat request}
   InitialRequest.Signature:=WORKER_SIGNATURE;
   InitialRequest.Interval:=0;
   InitialRequest.Flags:=WORKER_FLAG_NOFREE;
   InitialRequest.Lock:=INVALID_HANDLE_VALUE;
   InitialRequest.Timer:=INVALID_HANDLE_VALUE;
   InitialRequest.Task:=Task;
   InitialRequest.Data:=Data;
   InitialRequest.Callback:=Callback;
  end;

 {Check Interval}
 if Interval = 0 then
  begin
   {Submit Worker Request}
   FillChar(Message,SizeOf(TMessage),0);
   Message.Msg:=PtrUInt(WorkerRequest);
   {Check Flags}
   if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
    begin
     if MessageslotSend(WorkerMessageslot,Message) <> ERROR_SUCCESS then
      begin
       {Free Worker Request}
       FreeMem(WorkerRequest);
       Exit;
      end;
    end
   else
    begin
     if MessageslotSend(WorkerPriorityMessageslot,Message) <> ERROR_SUCCESS then
      begin
       {Free Worker Request}
       FreeMem(WorkerRequest);
       Exit;
      end;
    end;

   {Return Result}
   Result:=TWorkerHandle(ERROR_SUCCESS);

   {Worker Request will be freed by WorkerExecute/WorkerPriorityExecute}
  end
 else
  begin
   {Check Reschedule}
   if (WorkerRequest.Flags and WORKER_FLAG_RESCHEDULE) = 0 then
    begin
     {Create Timer (Once)}
     WorkerRequest.Timer:=TimerCreateEx(WorkerRequest.Interval,TIMER_STATE_ENABLED,TIMER_FLAG_NONE,TTimerEvent(WorkerTimer),WorkerRequest);
     if WorkerRequest.Timer = INVALID_HANDLE_VALUE then
      begin
       {Free Worker Request}
       FreeMem(WorkerRequest);
       Exit;
      end;

     {Return Result}
     Result:=TWorkerHandle(ERROR_SUCCESS);

     {Timer will be destroyed by WorkerTimer}
     {Worker Request will be freed by WorkerExecute/WorkerPriorityExecute}
    end
   else
    begin
     {Check Immediate}
     if (WorkerRequest.Flags and WORKER_FLAG_IMMEDIATE) <> 0 then
      begin
       {Get Initial Request}
       InitialRequest:=PWorkerRequest(PtrUInt(WorkerRequest) + PtrUInt(SizeOf(TWorkerRequest)));
       if (InitialRequest <> nil) and (InitialRequest.Signature = WORKER_SIGNATURE) then
        begin
         {Submit Initial Request}
         FillChar(Message,SizeOf(TMessage),0);
         Message.Msg:=PtrUInt(InitialRequest);
         {Check Flags}
         if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
          begin
           MessageslotSend(WorkerMessageslot,Message);
          end
         else
          begin
           MessageslotSend(WorkerPriorityMessageslot,Message);
          end;

         {Initial Request will NOT be freed by WorkerExecute/WorkerPriorityExecute}
        end;
      end;

     {Create Lock}
     WorkerRequest.Lock:=SpinCreate;
     if WorkerRequest.Lock = INVALID_HANDLE_VALUE then
      begin
       {Free Worker Request}
       FreeMem(WorkerRequest);
       Exit;
      end;

     {Create Timer (Repeating)}
     WorkerRequest.Timer:=TimerCreateEx(WorkerRequest.Interval,TIMER_STATE_ENABLED,TIMER_FLAG_RESCHEDULE,TTimerEvent(WorkerTimer),WorkerRequest);
     if WorkerRequest.Timer = INVALID_HANDLE_VALUE then
      begin
       {Free Request Lock}
       SpinDestroy(WorkerRequest.Lock);

       {Free Worker Request}
       FreeMem(WorkerRequest);
       Exit;
      end;

     {Return Result}
     Result:=TWorkerHandle(WorkerRequest);

     {Lock will be destroyed by WorkerTimer when request is cancelled}
     {Timer will be destroyed by WorkerTimer when request is cancelled}
     {Worker Request will be freed by WorkerTimer when request is cancelled}
     {Initial/Repeat Request will be freed by WorkerTimer when request is cancelled}
    end;
  end;
end;

{==============================================================================}

function WorkerCancel(Worker:TWorkerHandle):LongWord;
{Cancel a previously scheduled worker thread task}
{Worker: The handle of the worker task to cancel}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Worker}
 if Worker = INVALID_HANDLE_VALUE then Exit;
 if Worker = TWorkerHandle(ERROR_SUCCESS) then Exit;

 {Check Request}
 WorkerRequest:=PWorkerRequest(Worker);
 if WorkerRequest = nil then Exit;
 if WorkerRequest.Signature <> WORKER_SIGNATURE then Exit;

 {Acquire the Lock}
 if SpinLock(WorkerRequest.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Signature}
    if WorkerRequest.Signature <> WORKER_SIGNATURE then Exit;

    {Set Cancelled}
    WorkerRequest.Flags:=WorkerRequest.Flags or WORKER_FLAG_CANCEL;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    SpinUnlock(WorkerRequest.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function WorkerScheduleIRQ(Affinity:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
{Schedule a task to be performed by a worker thread when the caller is an IRQ handler}
{Affinity: CPU Affinity for memory allocation (eg CPU_AFFINITY_0 or CPU_AFFINITY_NONE)}
{Task: The function to be called by the worker}
{Data: A pointer to user defined data which will be passed to the task function (Optional)}
{Callback: The function to be called by the worker when the task has completed (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The task will be performed immediately, for delayed tasks etc see WorkerSchedule(Ex)}
begin
 {}
 Result:=WorkerScheduleIRQEx(Affinity,WORKER_FLAG_NONE,Task,Data,Callback);
end;

{==============================================================================}

function WorkerScheduleIRQEx(Affinity,Flags:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord;
{Schedule a task to be performed by a worker thread when the caller is an IRQ handler}
{Affinity: CPU Affinity for memory allocation (eg CPU_AFFINITY_0 or CPU_AFFINITY_NONE)}
{Flags: The flags for the task (eg WORKER_FLAG_PRIORITY)}
{Task: The function to be called by the worker}
{Data: A pointer to user defined data which will be passed to the task function (Optional)}
{Callback: The function to be called by the worker when the task has completed (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The task will be performed immediately, for delayed tasks etc see WorkerSchedule(Ex)}
var
 Message:TMessage;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Task}
 if not Assigned(Task) then Exit;

 {Check Data}
 {if Data = nil then Exit;} {May be nil}

 {Check Callback}
 {if not Assigned(Callback) then Exit;}  {May be nil}

 {Check Flags (Excluded)}
 if (Flags and WORKER_FLAG_EXCLUDED_IRQ) <> 0 then Exit;

 {Check Flags (Internal)}
 if (Flags and WORKER_FLAG_INTERNAL) <> 0 then Exit;

 {Create Worker Request}
 WorkerRequest:=AllocIRQMem(SizeOf(TWorkerRequest),Affinity);
 if WorkerRequest = nil then
  begin
   Result:=ERROR_NOT_ENOUGH_MEMORY;
   Exit;
  end;

 {Setup Worker Request}
 WorkerRequest.Signature:=WORKER_SIGNATURE;
 WorkerRequest.Interval:=0;
 WorkerRequest.Flags:=Flags or WORKER_FLAG_IRQ;
 WorkerRequest.Lock:=INVALID_HANDLE_VALUE;
 WorkerRequest.Timer:=INVALID_HANDLE_VALUE;
 WorkerRequest.Task:=Task;
 WorkerRequest.Data:=Data;
 WorkerRequest.Callback:=Callback;

 {Flush Worker Request}
 if not(HEAP_IRQ_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(WorkerRequest),SizeOf(TWorkerRequest));
  end;

 {Submit Worker Request}
 FillChar(Message,SizeOf(TMessage),0);
 Message.Msg:=PtrUInt(WorkerRequest);
 {Check Flags}
 if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
  begin
   if MessageslotSend(WorkerMessageslot,Message) <> ERROR_SUCCESS then
    begin
     {Free Worker Request}
     FreeIRQMem(WorkerRequest);

     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end
 else
  begin
   if MessageslotSend(WorkerPriorityMessageslot,Message) <> ERROR_SUCCESS then
    begin
     {Free Worker Request}
     FreeIRQMem(WorkerRequest);

     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;

 {Worker Request will be freed by WorkerExecute/WorkerPriorityExecute}
end;

{==============================================================================}

function WorkerScheduleFIQ(Affinity:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
{Schedule a task to be performed by a worker thread when the caller is an FIQ handler}
{Affinity: CPU Affinity for memory allocation (eg CPU_AFFINITY_0 or CPU_AFFINITY_NONE)}
{Task: The function to be called by the worker}
{Data: A pointer to user defined data which will be passed to the task function (Optional)}
{Callback: The function to be called by the worker when the task has completed (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The task will be performed immediately, for delayed tasks etc see WorkerSchedule(Ex)}
begin
 {}
 Result:=WorkerScheduleFIQEx(Affinity,WORKER_FLAG_NONE,Task,Data,Callback);
end;

{==============================================================================}

function WorkerScheduleFIQEx(Affinity,Flags:LongWord;Task:TWorkerTask;Data:Pointer;Callback:TWorkerCallback):LongWord;
{Schedule a task to be performed by a worker thread when the caller is an FIQ handler}
{Affinity: CPU Affinity for memory allocation (eg CPU_AFFINITY_0 or CPU_AFFINITY_NONE)}
{Flags: The flags for the task (eg WORKER_FLAG_PRIORITY)}
{Task: The function to be called by the worker}
{Data: A pointer to user defined data which will be passed to the task function (Optional)}
{Callback: The function to be called by the worker when the task has completed (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The task will be performed immediately, for delayed tasks etc see WorkerSchedule(Ex)}
var
 Message:TMessage;
 WorkerRequest:PWorkerRequest;
 MessageslotHandle:TMessageslotHandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Task}
 if not Assigned(Task) then Exit;

 {Check Data}
 {if Data = nil then Exit;} {May be nil}

 {Check Callback}
 {if not Assigned(Callback) then Exit;}  {May be nil}

 {Check Flags (Excluded)}
 if (Flags and WORKER_FLAG_EXCLUDED_FIQ) <> 0 then Exit;

 {Check Flags (Internal)}
 if (Flags and WORKER_FLAG_INTERNAL) <> 0 then Exit;

 {Create Worker Request}
 WorkerRequest:=AllocFIQMem(SizeOf(TWorkerRequest),Affinity);
 if WorkerRequest = nil then
  begin
   Result:=ERROR_NOT_ENOUGH_MEMORY;
   Exit;
  end;

 {Setup Worker Request}
 WorkerRequest.Signature:=WORKER_SIGNATURE;
 WorkerRequest.Interval:=0;
 WorkerRequest.Flags:=Flags or WORKER_FLAG_FIQ;
 WorkerRequest.Lock:=INVALID_HANDLE_VALUE;
 WorkerRequest.Timer:=INVALID_HANDLE_VALUE;
 WorkerRequest.Task:=Task;
 WorkerRequest.Data:=Data;
 WorkerRequest.Callback:=Callback;

 {Flush Worker Request}
 if not(HEAP_FIQ_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(WorkerRequest),SizeOf(TWorkerRequest));
  end;

 {Submit Worker Request}
 FillChar(Message,SizeOf(TMessage),0);
 Message.Msg:=PtrUInt(WorkerRequest);
 {Check Flags}
 if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
  begin
   MessageslotHandle:=WorkerMessageslot;
  end
 else
  begin
   MessageslotHandle:=WorkerPriorityMessageslot;
  end;

 if SCHEDULER_FIQ_ENABLED then
  begin
   if MessageslotSend(MessageslotHandle,Message) <> ERROR_SUCCESS then
    begin
     {Free Worker Request}
     FreeFIQMem(WorkerRequest);

     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end
 else
  begin
   if TaskerMessageslotSend(MessageslotHandle,Message) <> ERROR_SUCCESS then
    begin
     {Free Worker Request}
     FreeFIQMem(WorkerRequest);

     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;

 {Worker Request will be freed by WorkerExecute/WorkerPriorityExecute}
end;

{==============================================================================}

function WorkerIncrease(Count:LongWord):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
{Increase the number of worker threads available}
{Count: Number of worker threads to increase by}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=WorkerIncreaseEx(Count,False);
end;

{==============================================================================}

function WorkerIncreaseEx(Count:LongWord;Priority:Boolean):LongWord;
{Increase the number of worker threads available}
{Count: Number of worker threads to increase by}
{Priority: If true increase worker priority threads}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Counter:LongWord;
 Thread:TThreadHandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Count}
 if Count < 1 then Exit;

 {Check Priority}
 if not Priority then
  begin
   {Acquire Lock}
   if SpinLock(WorkerThreadLock) = ERROR_SUCCESS then
    begin
     try
      {Create Worker Threads}
      for Counter:=0 to Count - 1 do
       begin
        {Create Worker Thread}
        Thread:=BeginThread(WorkerExecute,nil,Thread,THREAD_STACK_DEFAULT_SIZE);
        if Thread = INVALID_HANDLE_VALUE then
         begin
          {$IFDEF THREAD_DEBUG}
          if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to increase worker threads');
          {$ENDIF}

          Result:=ERROR_OPERATION_FAILED;
          Exit;
         end;
        {Setup Worker Thread}
        {Name}
        ThreadSetName(Thread,WORKER_THREAD_NAME + IntToStr(WorkerThreadNext));
        {Priority}
        ThreadSetPriority(Thread,WORKER_THREAD_PRIORITY);
        Inc(WorkerThreadNext);
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release Lock}
      SpinUnlock(WorkerThreadLock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin
   {Acquire Lock}
   if SpinLock(WorkerPriorityThreadLock) = ERROR_SUCCESS then
    begin
     try
      {Create Worker Priority Threads}
      for Counter:=0 to Count - 1 do
       begin
        {Create Worker Priority Thread}
        Thread:=BeginThread(WorkerPriorityExecute,nil,Thread,THREAD_STACK_DEFAULT_SIZE);
        if Thread = INVALID_HANDLE_VALUE then
         begin
          {$IFDEF THREAD_DEBUG}
          if THREAD_LOG_ENABLED then ThreadLogDebug('Failed to increase worker priority threads');
          {$ENDIF}

          Result:=ERROR_OPERATION_FAILED;
          Exit;
         end;
        {Setup Worker Priority Thread}
        {Name}
        ThreadSetName(Thread,WORKER_PRIORITY_THREAD_NAME + IntToStr(WorkerPriorityThreadNext));
        {Priority}
        ThreadSetPriority(Thread,WORKER_PRIORITY_THREAD_PRIORITY);
        Inc(WorkerPriorityThreadNext);
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release Lock}
      SpinUnlock(WorkerPriorityThreadLock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function WorkerDecrease(Count:LongWord):LongWord; {$IFDEF WORKER_INLINE}inline;{$ENDIF}
{Decrease the number of worker threads available}
{Count: Number of worker threads to decrease by}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=WorkerDecreaseEx(Count,False);
end;

{==============================================================================}

function WorkerDecreaseEx(Count:LongWord;Priority:Boolean):LongWord;
{Decrease the number of worker threads available}
{Count: Number of worker threads to decrease by}
{Priority: If true decrease worker priority threads}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Counter:LongWord;
 Message:TMessage;
 WorkerRequest:PWorkerRequest;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Count}
 if Count < 1 then Exit;

 {Check Priority}
 if not Priority then
  begin
   {Acquire Lock}
   if SpinLock(WorkerThreadLock) = ERROR_SUCCESS then
    begin
     try
      {Check Count}
      if WORKER_THREAD_COUNT > WorkerThreadCount then Exit;
      if Count > (WorkerThreadCount - WORKER_THREAD_COUNT) then Exit;

      {Terminate Worker Threads}
      for Counter:=0 to Count - 1 do
       begin
        {Create Worker Request}
        WorkerRequest:=AllocMem(SizeOf(TWorkerRequest));
        if WorkerRequest = nil then Exit;

        {Setup Worker Request}
        WorkerRequest.Signature:=WORKER_SIGNATURE;
        WorkerRequest.Interval:=0;
        WorkerRequest.Flags:=WORKER_FLAG_TERMINATE;
        WorkerRequest.Lock:=INVALID_HANDLE_VALUE;
        WorkerRequest.Timer:=INVALID_HANDLE_VALUE;
        WorkerRequest.Task:=nil;
        WorkerRequest.Data:=nil;
        WorkerRequest.Callback:=nil;

        {Submit Worker Request}
        FillChar(Message,SizeOf(TMessage),0);
        Message.Msg:=PtrUInt(WorkerRequest);
        if MessageslotSend(WorkerMessageslot,Message) <> ERROR_SUCCESS then
         begin
          {Free Worker Request}
          FreeMem(WorkerRequest);

          Result:=ERROR_OPERATION_FAILED;
          Exit;
         end;

        {Worker Request will be freed by WorkerExecute}
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release Lock}
      SpinUnlock(WorkerThreadLock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin
   {Acquire Lock}
   if SpinLock(WorkerPriorityThreadLock) = ERROR_SUCCESS then
    begin
     try
      {Check Count}
      if WORKER_PRIORITY_THREAD_COUNT > WorkerPriorityThreadCount then Exit;
      if Count > (WorkerPriorityThreadCount - WORKER_PRIORITY_THREAD_COUNT) then Exit;

      {Terminate Worker Priority Threads}
      for Counter:=0 to Count - 1 do
       begin
        {Create Worker Request}
        WorkerRequest:=AllocMem(SizeOf(TWorkerRequest));
        if WorkerRequest = nil then Exit;

        {Setup Worker Request}
        WorkerRequest.Signature:=WORKER_SIGNATURE;
        WorkerRequest.Interval:=0;
        WorkerRequest.Flags:=WORKER_FLAG_TERMINATE;
        WorkerRequest.Lock:=INVALID_HANDLE_VALUE;
        WorkerRequest.Timer:=INVALID_HANDLE_VALUE;
        WorkerRequest.Task:=nil;
        WorkerRequest.Data:=nil;
        WorkerRequest.Callback:=nil;

        {Submit Worker Request}
        FillChar(Message,SizeOf(TMessage),0);
        Message.Msg:=PtrUInt(WorkerRequest);
        if MessageslotSend(WorkerPriorityMessageslot,Message) <> ERROR_SUCCESS then
         begin
          {Free Worker Request}
          FreeMem(WorkerRequest);

          Result:=ERROR_OPERATION_FAILED;
          Exit;
         end;

        {Worker Request will be freed by WorkerPriorityExecute}
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release Lock}
      SpinUnlock(WorkerPriorityThreadLock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

procedure WorkerTimer(WorkerRequest:PWorkerRequest);
{Procedure called internally to schedule worker threads}

{Note: Not intended to be called directly by applications}
var
 Message:TMessage;
 RepeatRequest:PWorkerRequest;
begin
 {}
 {Check Worker Request}
 if WorkerRequest = nil then Exit;
 if WorkerRequest.Signature <> WORKER_SIGNATURE then Exit;

 {Check Reschedule}
 if (WorkerRequest.Flags and WORKER_FLAG_RESCHEDULE) = 0 then
  begin
   {Free Request Timer}
   if WorkerRequest.Timer <> INVALID_HANDLE_VALUE then
    begin
     TimerDestroy(WorkerRequest.Timer);
    end;

   {Submit Worker Request}
   FillChar(Message,SizeOf(TMessage),0);
   Message.Msg:=PtrUInt(WorkerRequest);
   {Check Flags}
   if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
    begin
     if MessageslotSend(WorkerMessageslot,Message) <> ERROR_SUCCESS then
      begin
       {Free Worker Request}
       FreeMem(WorkerRequest);
      end;
    end
   else
    begin
     if MessageslotSend(WorkerPriorityMessageslot,Message) <> ERROR_SUCCESS then
      begin
       {Free Worker Request}
       FreeMem(WorkerRequest);
      end;
    end;

   {Worker Request will be freed by WorkerExecute/WorkerPriorityExecute}
  end
 else
  begin
   {Check Cancel}
   if (WorkerRequest.Flags and WORKER_FLAG_CANCEL) <> 0 then
    begin
     {Acquire the Lock}
     if SpinLock(WorkerRequest.Lock) = ERROR_SUCCESS then
      begin
       {Check Signature}
       if WorkerRequest.Signature <> WORKER_SIGNATURE then
        begin
         {Release the Lock}
         SpinUnlock(WorkerRequest.Lock);

         Exit;
        end;

       {Invalidate Worker Request}
       WorkerRequest.Signature:=0;

       {Release the Lock}
       SpinUnlock(WorkerRequest.Lock);

       {Free Request Timer}
       if WorkerRequest.Timer <> INVALID_HANDLE_VALUE then
        begin
         TimerDestroy(WorkerRequest.Timer);
        end;

       {Free Request Lock}
       SpinDestroy(WorkerRequest.Lock);

       {Free Worker Request}
       FreeMem(WorkerRequest);
      end;
    end
   else
    begin
     {Acquire the Lock}
     if SpinLock(WorkerRequest.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Signature}
        if WorkerRequest.Signature <> WORKER_SIGNATURE then Exit;

        {Get Repeat Request}
        RepeatRequest:=PWorkerRequest(PtrUInt(WorkerRequest) + PtrUInt(SizeOf(TWorkerRequest)));
        if (RepeatRequest <> nil) and (RepeatRequest.Signature = WORKER_SIGNATURE) then
         begin
          {Submit Repeat Request}
          FillChar(Message,SizeOf(TMessage),0);
          Message.Msg:=PtrUInt(RepeatRequest);
          {Check Flags}
          if (WorkerRequest.Flags and WORKER_FLAG_PRIORITY) = 0 then
           begin
            MessageslotSend(WorkerMessageslot,Message);
           end
          else
           begin
            MessageslotSend(WorkerPriorityMessageslot,Message);
           end;

          {Repeat Request will NOT be freed by WorkerExecute/WorkerPriorityExecute}
         end;
       finally
        {Release the Lock}
        SpinUnlock(WorkerRequest.Lock);
       end;
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Tasker Functions}
function TaskerThreadSendMessage(Thread:TThreadHandle;const Message:TMessage):LongWord;
{Perform a ThreadSendMessage() function call using the tasker list}
var
 Task:PTaskerThreadSendMessage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Scheduler}
 if SCHEDULER_FIQ_ENABLED then
  begin
   {Can call ThreadSendMessage from FIQ if scheduler uses FIQ}
   Result:=ThreadSendMessage(Thread,Message);

   Exit;
  end;

 {Check Thread}
 if Thread = INVALID_HANDLE_VALUE then Exit;

 {Create Task}
 Task:=AllocFIQMem(SizeOf(TTaskerThreadSendMessage),CPU_AFFINITY_NONE);
 if Task = nil then Exit;

 {Update Task}
 Task.Task:=TASKER_TASK_THREADSENDMESSAGE;
 Task.Thread:=Thread;
 Task.Message:=Message;

 {Flush Task}
 if not(HEAP_FIQ_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(Task),SizeOf(TTaskerThreadSendMessage));
  end;

 {Enqueue}
 Result:=TaskerEnqueue(PTaskerTask(Task));
 if Result <> ERROR_SUCCESS then
  begin
   {Free Task}
   FreeFIQMem(Task)
  end;

 {Task will be freed by TaskerTrigger}
end;

{==============================================================================}

function TaskerMessageslotSend(Messageslot:TMessageslotHandle;const Message:TMessage):LongWord;
{Perform a MessageslotSend() function call using the tasker list}
var
 Task:PTaskerMessageslotSend;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Scheduler}
 if SCHEDULER_FIQ_ENABLED then
  begin
   {Can call MessageslotSend from FIQ if scheduler uses FIQ}
   Result:=MessageslotSend(Messageslot,Message);

   Exit;
  end;

 {Check Messageslot}
 if Messageslot = INVALID_HANDLE_VALUE then Exit;

 {Create Task}
 Task:=AllocFIQMem(SizeOf(TTaskerMessageslotSend),CPU_AFFINITY_NONE);
 if Task = nil then Exit;

 {Update Task}
 Task.Task:=TASKER_TASK_MESSAGESLOTSEND;
 Task.Messageslot:=Messageslot;
 Task.Message:=Message;

 {Flush Task}
 if not(HEAP_FIQ_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(Task),SizeOf(TTaskerMessageslotSend));
  end;

 {Enqueue}
 Result:=TaskerEnqueue(PTaskerTask(Task));
 if Result <> ERROR_SUCCESS then
  begin
   {Free Task}
   FreeFIQMem(Task)
  end;

 {Task will be freed by TaskerTrigger}
end;

{==============================================================================}

function TaskerSemaphoreSignal(Semaphore:TSemaphoreHandle;Count:LongWord):LongWord;
{Perform a SemaphoreSignal() function call using the tasker list}
var
 Task:PTaskerSemaphoreSignal;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Scheduler}
 if SCHEDULER_FIQ_ENABLED then
  begin
   {Can call SemaphoreSignal from FIQ if scheduler uses FIQ}
   Result:=SemaphoreSignalEx(Semaphore,Count,nil);

   Exit;
  end;

 {Check Semaphore}
 if Semaphore = INVALID_HANDLE_VALUE then Exit;

 {Create Task}
 Task:=AllocFIQMem(SizeOf(TTaskerSemaphoreSignal),CPU_AFFINITY_NONE);
 if Task = nil then Exit;

 {Update Task}
 Task.Task:=TASKER_TASK_SEMAPHORESIGNAL;
 Task.Semaphore:=Semaphore;
 Task.Count:=Count;

 {Flush Task}
 if not(HEAP_FIQ_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(Task),SizeOf(TTaskerSemaphoreSignal));
  end;

 {Enqueue}
 Result:=TaskerEnqueue(PTaskerTask(Task));
 if Result <> ERROR_SUCCESS then
  begin
   {Free Task}
   FreeFIQMem(Task)
  end;

 {Task will be freed by TaskerTrigger}
end;

{==============================================================================}

function TaskerCompletionReset(Completion:TCompletionHandle):LongWord;
{Perform a CompletionReset() function call using the tasker list}
var
 Task:PTaskerCompletionReset;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Scheduler}
 if SCHEDULER_FIQ_ENABLED then
  begin
   {Can call CompletionReset from FIQ if scheduler uses FIQ}
   Result:=CompletionReset(Completion);

   Exit;
  end;

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Create Task}
 Task:=AllocFIQMem(SizeOf(TTaskerCompletionReset),CPU_AFFINITY_NONE);
 if Task = nil then Exit;

 {Update Task}
 Task.Task:=TASKER_TASK_COMPLETIONRESET;
 Task.Completion:=Completion;

 {Flush Task}
 if not(HEAP_FIQ_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(Task),SizeOf(TTaskerCompletionReset));
  end;

 {Enqueue}
 Result:=TaskerEnqueue(PTaskerTask(Task));
 if Result <> ERROR_SUCCESS then
  begin
   {Free Task}
   FreeFIQMem(Task)
  end;

 {Task will be freed by TaskerTrigger}
end;

{==============================================================================}

function TaskerCompletionComplete(Completion:TCompletionHandle;All:Boolean):LongWord;
{Perform a CompletionComplete() or CompletionCompleteAll() function call using the tasker list}
var
 Task:PTaskerCompletionComplete;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Scheduler}
 if SCHEDULER_FIQ_ENABLED then
  begin
   {Can call CompletionComplete or CompletionCompleteAll from FIQ if scheduler uses FIQ}
   if All then
    begin
     Result:=CompletionCompleteAll(Completion);
    end
   else
    begin
     Result:=CompletionComplete(Completion);
    end;

   Exit;
  end;

 {Check Completion}
 if Completion = INVALID_HANDLE_VALUE then Exit;

 {Create Task}
 Task:=AllocFIQMem(SizeOf(TTaskerCompletionComplete),CPU_AFFINITY_NONE);
 if Task = nil then Exit;

 {Update Task}
 Task.Task:=TASKER_TASK_COMPLETIONCOMPLETE;
 Task.Completion:=Completion;
 Task.All:=All;

 {Flush Task}
 if not(HEAP_FIQ_CACHE_COHERENT) then
  begin
   CleanDataCacheRange(PtrUInt(Task),SizeOf(TTaskerCompletionComplete));
  end;

 {Enqueue}
 Result:=TaskerEnqueue(PTaskerTask(Task));
 if Result <> ERROR_SUCCESS then
  begin
   {Free Task}
   FreeFIQMem(Task)
  end;

 {Task will be freed by TaskerTrigger}
end;

{==============================================================================}

function TaskerEnqueue(Task:PTaskerTask):LongWord;
{Add the supplied task to the end of the Tasker list}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Task}
 if Task = nil then Exit;

 {Check the List}
 if TaskerList = nil then Exit;

 {Lock the List}
 if SpinLockIRQFIQ(TaskerList.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Last}
    if TaskerList.Last = nil then
     begin
      Task.Next:=nil;
      Task.Prev:=nil;
      TaskerList.First:=Task;
      TaskerList.Last:=Task;
     end
    else
     begin
      Task.Next:=nil;
      Task.Prev:=TaskerList.Last;
      TaskerList.Last.Next:=Task;
      TaskerList.Last:=Task;
     end;

    {Increment Count}
    Inc(TaskerList.Count);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock the List}
    SpinUnlockIRQFIQ(TaskerList.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TaskerDequeue:PTaskerTask;
{Get and remove the first task from the Tasker list}
{Return: Dequeued Task or nil on failure (or list empty)}
var
 Task:PTaskerTask;
begin
 {}
 Result:=nil;

 {Check the List}
 if TaskerList = nil then Exit;

 {Lock the List}
 if SpinLockIRQFIQ(TaskerList.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Task (First)}
    Task:=TaskerList.First;
    if Task <> nil then
     begin
      {Remove First}
      TaskerList.First:=Task.Next;
      {Check Next}
      if Task.Next = nil then
       begin
        TaskerList.Last:=nil;
       end
      else
       begin
        Task.Next.Prev:=nil;
       end;

      {Decrement Count}
      Dec(TaskerList.Count);

      {Return Result}
      Result:=Task;
     end;
   finally
    {Unlock the List}
    SpinUnlockIRQFIQ(TaskerList.Lock);
   end;
  end;
end;

{==============================================================================}

function TaskerCheck:LongWord;
{Check if the tasker list is empty or contains tasks}
{Return: ERROR_SUCCESS if the list contains tasks, ERROR_NO_MORE_ITEMS if list is empty or another error code on failure}
{Note: Called by clock interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Handler}
 if Assigned(TaskerCheckHandler) then
  begin
   {Use the Handler method}
   Result:=TaskerCheckHandler;
  end
 else
  begin
   {Use the Default method}
   {Check the List}
   if TaskerList = nil then Exit;

   {Setup Result}
   Result:=ERROR_NO_MORE_ITEMS;

   {Check Tasker List}
   if TaskerList.First <> nil then
    begin
     Result:=ERROR_SUCCESS;
    end;
  end;
end;

{==============================================================================}

function TaskerTrigger:LongWord;
{Dequeue all tasks in the tasker list and perform the requested task for each}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Called by clock interrupt with IRQ or FIQ disabled and running on the IRQ or FIQ thread}
var
 Task:PTaskerTask;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check the Handler}
 if Assigned(TaskerTriggerHandler) then
  begin
   {Use the Handler method}
   Result:=TaskerTriggerHandler;
  end
 else
  begin
   {Use the Default method}
   {Dequeue Task}
   Task:=TaskerDequeue;
   while Task <> nil do
    begin
     {Check Task}
     case Task.Task of
      TASKER_TASK_THREADSENDMESSAGE:begin
        {Call ThreadSendMessage}
        ThreadSendMessage(PTaskerThreadSendMessage(Task).Thread,PTaskerThreadSendMessage(Task).Message);
       end;
      TASKER_TASK_MESSAGESLOTSEND:begin
        {Call MessageslotSend}
        MessageslotSend(PTaskerMessageslotSend(Task).Messageslot,PTaskerMessageslotSend(Task).Message);
       end;
      TASKER_TASK_SEMAPHORESIGNAL:begin
        {Call SemaphoreSignal}
        SemaphoreSignalEx(PTaskerSemaphoreSignal(Task).Semaphore,PTaskerSemaphoreSignal(Task).Count,nil);
       end;
      TASKER_TASK_COMPLETIONRESET:begin
        {Call CompletionReset}
        CompletionReset(PTaskerCompletionReset(Task).Completion);
       end;
      TASKER_TASK_COMPLETIONCOMPLETE:begin
        {Check All}
        if PTaskerCompletionComplete(Task).All then
         begin
          {Call CompletionCompleteAll}
          CompletionCompleteAll(PTaskerCompletionComplete(Task).Completion);
         end
        else
         begin
          {Call CompletionComplete}
          CompletionComplete(PTaskerCompletionComplete(Task).Completion);
         end;
       end;
     end;

     {Free Task}
     FreeFIQMem(Task);

     {Dequeue Task}
     Task:=TaskerDequeue;
    end;

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}
{==============================================================================}
{RTL Init/Exit Functions}
procedure SysInitProc;
{Called at the end of FPC_INITIALIZEUNITS after all units have been initialized}
begin
 {}
 SysInitializationCompleted:=True;

 {Check Last InitProc}
 if Assigned(SysLastInitProc) then
  begin
   TProcedure(SysLastInitProc);
  end;
end;

{==============================================================================}

procedure SysExitProc;
{Called by InternalExit before FinalizeUnits and other shutdown procedures}
{Also may be called via Halt from RunError and other error handling}
begin
 {}
 ThreadHalt(ERROR_RUNTIME_ERROR);
end;

{==============================================================================}

function SysInitCompleted:Boolean;
begin
 {}
 Result:=SysInitializationCompleted;
end;

{==============================================================================}
{==============================================================================}
{RTL Process Functions}
function SysGetProcessID:SizeUInt;
{Return current Thread ID}
begin
 {}
 Result:=ThreadGetCurrent;
end;

{==============================================================================}
{==============================================================================}
{RTL Thread Manager Functions}
function ThreadMain(Parameter:Pointer):PtrInt;
{See: ThreadMain in \source\rtl\win\systhrd.inc}
{See also: ThreadMain in \source\rtl\unix\cthreads.pp}
{See also: ThreadMain in \source\rtl\netware\systhrd.inc}
var
 ThreadInfo:TThreadInfo;
begin
 {}
 Result:=0;

 {Allocate Thread Vars, this must be the first thing, because the exception management and IO depends on ThreadVars}
 SysAllocateThreadVars;

 {Copy Thread Info Block to Stack}
 ThreadInfo:=PThreadInfo(Parameter)^;

 {Free the Parameter}
 {FreeMem(Parameter);} {Moved below to allow for Idle/IRQ/FIQ Threads}

 {Initialize Thread}
 InitThread(ThreadInfo.StackLength);

 {Initialize Standard Text IO (Input/Output/ErrOutput/StdOut/StdErr)}
 {Note: Normally done by InitThread}
 TextIOOpen(Input,TextIOWriteChar,TextIOReadChar,fmInput,nil);
 TextIOOpen(Output,TextIOWriteChar,TextIOReadChar,fmOutput,nil);
 TextIOOpen(ErrOutput,TextIOWriteChar,TextIOReadChar,fmOutput,nil);
 TextIOOpen(StdOut,TextIOWriteChar,TextIOReadChar,fmOutput,nil);
 TextIOOpen(StdErr,TextIOWriteChar,TextIOReadChar,fmOutput,nil);

 {Initialize InOutRes}
 {Note: Normally done by InitThread}
 InOutRes:=0;

 {Initialize Stack Checking}
 {Note: Normally done by InitThread}
 {StackLength:=CheckInitialStkLen(ThreadInfo.StackLength);}
 {StackBottom:=Sptr - StackLength;}

 {Start Thread Function}
 Result:=ThreadInfo.ThreadFunction(ThreadInfo.ThreadParameter);

 {Free the Parameter}
 FreeMem(Parameter);

 {Finalize Thread}
 DoneThread;

 {Thread Result}
 ThreadEnd(Result);
end;

{==============================================================================}

function SysBeginThread(SignalAction:Pointer;StackSize:PtrUInt;ThreadFunction:TThreadFunc;ThreadParameter:Pointer;CreationFlags:DWORD;var ThreadId:TThreadID):TThreadID;
{See: SysBeginThread in \source\rtl\win\systhrd.inc}
{See also: SysBeginThread in \source\rtl\netware\systhrd.inc}
{See also: CBeginThread in \source\rtl\unix\cthreads.pp}
{Note: SignalAction not used by Ultibo threading}
var
 ThreadInfo:PThreadInfo;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Initialization}
 if ThreadVarBlockSize = 0 then
  begin
   InitThreadVars(@SysRelocateThreadVar);
   IsMultiThread:=True;
  end;

 {Create the Thread Info Block}
 ThreadInfo:=AllocMem(SizeOf(TThreadInfo));
 ThreadInfo.ThreadFunction:=ThreadFunction;
 ThreadInfo.ThreadParameter:=ThreadParameter;
 ThreadInfo.StackLength:=StackSize;

 {Create the Thread}
 ThreadId:=ThreadCreate(ThreadMain,StackSize,THREAD_PRIORITY_DEFAULT,PChar(THREAD_NAME_DEFAULT),ThreadInfo);
 if ThreadId <> INVALID_HANDLE_VALUE then
  begin
   if (CreationFlags and THREAD_CREATE_SUSPENDED) = 0 then
    begin
     ThreadReady(ThreadId,False);
    end;
  end
 else
  begin
   FreeMem(ThreadInfo);
   ThreadId:=INVALID_HANDLE_VALUE;
  end;

 Result:=ThreadId;
end;

{==============================================================================}

function SysBeginThreadEx(SignalAction:Pointer;StackSize:PtrUInt;ThreadFunction:TThreadFunc;ThreadParameter:Pointer;CreationFlags:DWORD;Priority,Affinity,CPU:LongWord;Name:PChar;var ThreadId:TThreadID):TThreadID;
{See: SysBeginThread in \source\rtl\win\systhrd.inc}
{See also: SysBeginThread in \source\rtl\netware\systhrd.inc}
{See also: CBeginThread in \source\rtl\unix\cthreads.pp}
{Note: SignalAction not used by Ultibo threading}
var
 ThreadInfo:PThreadInfo;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Initialization}
 if ThreadVarBlockSize = 0 then
  begin
   InitThreadVars(@SysRelocateThreadVar);
   IsMultiThread:=True;
  end;

 {Create the Thread Info Block}
 ThreadInfo:=AllocMem(SizeOf(TThreadInfo));
 ThreadInfo.ThreadFunction:=ThreadFunction;
 ThreadInfo.ThreadParameter:=ThreadParameter;
 ThreadInfo.StackLength:=StackSize;

 {Create the Thread}
 ThreadId:=ThreadCreateEx(ThreadMain,StackSize,Priority,Affinity,CPU,Name,ThreadInfo);
 if ThreadId <> INVALID_HANDLE_VALUE then
  begin
   if (CreationFlags and THREAD_CREATE_SUSPENDED) = 0 then
    begin
     ThreadReady(ThreadId,False);
    end;
  end
 else
  begin
   FreeMem(ThreadInfo);
   ThreadId:=INVALID_HANDLE_VALUE;
  end;

 Result:=ThreadId;
end;

{==============================================================================}

procedure SysEndThread(ExitCode:DWORD);
{See: SysEndThread in \source\rtl\win\systhrd.inc}
{See also: SysEndThread in \source\rtl\netware\systhrd.inc}
{See also: CEndThread in \source\rtl\unix\cthreads.pp}
begin
 {}
 DoneThread;
 ThreadEnd(ExitCode);
end;

{==============================================================================}

function SysSuspendThread(ThreadHandle:TThreadID):DWORD;
begin
 {}
 Result:=DWORD(ThreadSuspend(ThreadHandle));
end;

{==============================================================================}

function SysResumeThread(ThreadHandle:TThreadID):DWORD;
begin
 {}
 Result:=DWORD(ThreadResume(ThreadHandle));
end;

{==============================================================================}

function SysKillThread(ThreadHandle:TThreadID):DWORD;
begin
 {}
 Result:=DWORD(ThreadTerminate(ThreadHandle,ERROR_SUCCESS));
end;

{==============================================================================}

function SysCloseThread(ThreadHandle:TThreadID):DWORD;
begin
 {Not currently implemented}
 Result:=0; //To Do //Could this be ThreadTerminate ? //Or is it for a different purpose ? //More like CloseHandle or HandleClose
                                  //For our case it is the same so we could just do the same as SysKillThread
                                  //Windows RTL does CloseHandle(), need to determine what will be the case
end;

{==============================================================================}

procedure SysThreadSwitch;
begin
 {}
 ThreadYield;
end;

{==============================================================================}

function SysWaitForThreadTerminate(ThreadHandle:TThreadID;TimeoutMs:LongInt):DWORD;  {0=no timeout}
begin
 {}
 if TimeoutMs = 0 then
  begin
   Dec(TimeoutMs); {Change to INFINITE}
  end;
 if ThreadWaitTerminate(ThreadHandle,TimeoutMs) = ERROR_SUCCESS then
  begin
   Result:=ThreadGetExitCode(ThreadHandle);
  end
 else
  begin
   Result:=ERROR_WAIT_TIMEOUT;
  end;
end;

{==============================================================================}

function SysThreadSetPriority(ThreadHandle:TThreadID;Priority:LongInt):Boolean;      {-15..+15, 0=normal}
var
 Value:LongWord;
begin
 {}
 Value:=THREAD_PRIORITY_NORMAL;

 {Map the RTL priority to Ultibo priority}
 case Priority of
  -15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3:Value:=THREAD_PRIORITY_IDLE;
  -2:Value:=THREAD_PRIORITY_LOWEST;
  -1:Value:=THREAD_PRIORITY_LOWER;
  0:Value:=THREAD_PRIORITY_NORMAL;
  1:Value:=THREAD_PRIORITY_HIGHER;
  2:Value:=THREAD_PRIORITY_HIGHEST;
  3,4,5,6,7,8,9,10,11,12,13,14,15:Value:=THREAD_PRIORITY_CRITICAL;
 end;

 {Set Priority}
 Result:=(ThreadSetPriority(ThreadHandle,Value) <> LongWord(INVALID_HANDLE_VALUE));
end;

{==============================================================================}

function SysThreadGetPriority(ThreadHandle:TThreadID):LongInt;
var
 Value:LongWord;
begin
 {}
 Result:=0;

 {Get Priority}
 Value:=ThreadGetPriority(ThreadHandle);

 {Map the Ultibo priority to RTL priority}
 case Value of
  THREAD_PRIORITY_IDLE:Result:=-15;
  THREAD_PRIORITY_LOWEST:Result:=-2;
  THREAD_PRIORITY_LOWER:Result:=-1;
  THREAD_PRIORITY_NORMAL:Result:=0;
  THREAD_PRIORITY_HIGHER:Result:=1;
  THREAD_PRIORITY_HIGHEST:Result:=2;
  THREAD_PRIORITY_CRITICAL:Result:=15;
 end;
end;

{==============================================================================}

function SysGetCurrentThreadId:TThreadID;
begin
 {}
 Result:=ThreadGetCurrent;
end;

{==============================================================================}

procedure SysSetThreadDebugNameA(ThreadHandle:TThreadID;const ThreadName:AnsiString);
begin
 {}
 ThreadSetName(ThreadHandle,ThreadName);
end;

{==============================================================================}

procedure SysSetThreadDebugNameU(ThreadHandle:TThreadID;const ThreadName:UnicodeString);
begin
 {}
 ThreadSetName(ThreadHandle,AnsiString(ThreadName));
end;

{==============================================================================}

procedure SysInitCriticalSection(var CriticalSection);
begin
 {}
 TRTLCriticalSection(CriticalSection).__m_owner:=Pointer(CriticalSectionCreate);
end;

{==============================================================================}

procedure SysDoneCriticalSection(var CriticalSection);
begin
 {}
 CriticalSectionDestroy(TCriticalSectionHandle(TRTLCriticalSection(CriticalSection).__m_owner));
end;

{==============================================================================}

procedure SysEnterCriticalSection(var CriticalSection);
begin
 {}
 CriticalSectionLockEx(TCriticalSectionHandle(TRTLCriticalSection(CriticalSection).__m_owner),INFINITE);
end;

{==============================================================================}

function SysTryEnterCriticalSection(var CriticalSection):LongInt;
{The return value is zero if another thread owns the critical section, or nonzero
 if the current thread already owns or succesfully obtained the critical section}
begin
 {}
 Result:=0;
 if CriticalSectionTryLock(TCriticalSectionHandle(TRTLCriticalSection(CriticalSection).__m_owner)) = ERROR_SUCCESS then
  begin
   Result:=1;
  end;
end;

{==============================================================================}

procedure SysLeaveCriticalSection(var CriticalSection);
begin
 {}
 CriticalSectionUnlock(TCriticalSectionHandle(TRTLCriticalSection(CriticalSection).__m_owner));
end;

{==============================================================================}

procedure SysInitThreadVar(var Offset:DWORD;Size:DWORD);
{See: CInitThreadvar in \source\rtl\unix\cthreads.pp}
{See also: SysInitThreadvar in \source\rtl\win\systhrd.inc}
{See also: SysInitThreadvar in \source\rtl\netware\systhrd.inc}
begin
 {}
 {Data must be allocated at aligned boundaries for some CPUs}
 ThreadVarBlockSize:=Align(ThreadVarBlockSize,THREADVAR_MIN_ALIGNMENT);
 Offset:=ThreadVarBlockSize;
 Inc(ThreadVarBlockSize,Size);
end;

{==============================================================================}

function SysRelocateThreadVar(Offset:DWORD):Pointer;
{See: SysRelocateThreadvar in \source\rtl\win\systhrd.inc}
{See also: SysRelocateThreadvar in \source\rtl\netware\systhrd.inc}
{See also: CRelocateThreadvar in \source\rtl\unix\cthreads.pp}
var
 TlsPointer:Pointer;
begin
 {}
 Result:=nil;
 if Offset > ThreadVarBlockSize then Exit;

 TlsPointer:=ThreadGetTlsPointer(ThreadGetCurrent);
 if TlsPointer <> nil then
  begin
   Result:=Pointer(PtrUInt(TlsPointer) + Offset);
  end;
end;

{==============================================================================}

procedure SysAllocateThreadVars;
{See: SysAllocateThreadVars in \source\rtl\win\systhrd.inc}
{See also: SysAllocateThreadVars in \source\rtl\netware\systhrd.inc}
{See also: CAllocateThreadVars in \source\rtl\unix\cthreads.pp}
begin
 {}
 {Handled by ThreadCreate/ThreadDestroy}
end;

{==============================================================================}

procedure SysReleaseThreadVars;
{See: SysReleaseThreadVars in \source\rtl\win\systhrd.inc}
{See also: SysReleaseThreadVars in \source\rtl\netware\systhrd.inc}
{See also: CReleaseThreadVars in \source\rtl\unix\cthreads.pp}
begin
 {}
 {Handled by ThreadCreate/ThreadDestroy}
end;

{==============================================================================}

function SysBasicEventCreate(EventAttributes:Pointer;AManualReset,InitialState:Boolean;const Name:AnsiString):PEventState;
begin
 {}
 Result:=PEventState(EventCreate(AManualReset,InitialState));
end;

{==============================================================================}

procedure SysBasicEventDestroy(State:PEventState);
begin
 {}
 EventDestroy(TEventHandle(State));
end;

{==============================================================================}

procedure SysBasicEventResetEvent(State:PEventState);
begin
 {}
 EventReset(TEventHandle(State));
end;

{==============================================================================}

procedure SysBasicEventSetEvent(State:PEventState);
begin
 {}
 EventSet(TEventHandle(State));
end;

{==============================================================================}
{$if defined(FPC_STABLE) or defined(FPC_FIXES) or defined(FPC_LEGACY)}
function SysBasicEventWaitFor(Timeout:Cardinal;State:PEventState):LongInt;
{$else}
function SysBasicEventWaitFor(Timeout:Cardinal;State:PEventState;UseComWait:Boolean=False):LongInt;
{$endif}
const
 wrSignaled  = 0;
 wrTimeout   = 1;
 wrAbandoned = 2;
 wrError     = 3;

var
 Status:LongWord;
begin
 {}
 Status:=EventWaitEx(TEventHandle(State),Timeout);
 case Status of
  ERROR_SUCCESS:Result:=wrSignaled;
  ERROR_WAIT_TIMEOUT:Result:=wrTimeout;
  ERROR_WAIT_ABANDONED:Result:=wrAbandoned;
 else
  begin
   Result:=wrError;
  end;
 end;
end;

{==============================================================================}

function SysRTLEventCreate:PRTLEvent;
begin
 {}
 Result:=PRTLEvent(EventCreate(False,False));
end;

{==============================================================================}

procedure SysRTLEventDestroy(AEvent:PRTLEvent);
begin
 {}
 EventDestroy(TEventHandle(AEvent));
end;

{==============================================================================}

procedure SysRTLEventSetEvent(AEvent:PRTLEvent);
begin
 {}
 EventSet(TEventHandle(AEvent));
end;

{==============================================================================}

procedure SysRTLEventResetEvent(AEvent:PRTLEvent);
begin
 {}
 EventReset(TEventHandle(AEvent));
end;

{==============================================================================}

procedure SysRTLEventWaitFor(AEvent:PRTLEvent);
begin
 {}
 EventWaitEx(TEventHandle(AEvent),INFINITE);
end;

{==============================================================================}

procedure SysRTLEventWaitForTimeout(AEvent:PRTLEvent;Timeout:LongInt);
begin
 {}
 EventWaitEx(TEventHandle(AEvent),Timeout);
end;

{==============================================================================}

function SysSemaphoreInit:Pointer;
begin
 {}
 Result:=Pointer(SemaphoreCreate(0));
end;

{==============================================================================}

procedure SysSemaphoreDestroy(const Semaphore:Pointer);
begin
 {}
 SemaphoreDestroy(TSemaphoreHandle(Semaphore));
end;

{==============================================================================}

procedure SysSemaphorePost(const Semaphore:Pointer);
begin
 {}
 SemaphoreSignal(TSemaphoreHandle(Semaphore));
end;

{==============================================================================}

procedure SysSemaphoreWait(const Semaphore:Pointer);
begin
 {}
 SemaphoreWaitEx(TSemaphoreHandle(Semaphore),INFINITE);
end;

{==============================================================================}
{==============================================================================}
{Thread Helper Functions}
function SpinGetCount:LongWord;
{Get the current spin lock count}
begin
 {}
 Result:=SpinTableCount;
end;

{==============================================================================}

function MutexGetCount:LongWord;
{Get the current mutex count}
begin
 {}
 Result:=MutexTableCount;
end;

{==============================================================================}

function CriticalSectionGetCount:LongWord;
{Get the current critical section count}
begin
 {}
 Result:=CriticalSectionTableCount;
end;

{==============================================================================}

function SemaphoreGetCount:LongWord;
{Get the current semaphore count}
begin
 {}
 Result:=SemaphoreTableCount;
end;

{==============================================================================}

function SynchronizerGetCount:LongWord;
{Get the current synchronizer count}
begin
 {}
 Result:=SynchronizerTableCount;
end;

{==============================================================================}

function ConditionGetCount:LongWord;
{Get the current condition count}
begin
 {}
 Result:=ConditionTableCount;
end;

{==============================================================================}

function CompletionGetCount:LongWord;
{Get the current completion count}
begin
 {}
 Result:=CompletionTableCount;
end;

{==============================================================================}

function ListGetCount:LongWord;
{Get the current list count}
begin
 {}
 Result:=ListTableCount;
end;

{==============================================================================}

function QueueGetCount:LongWord;
{Get the current queue count}
begin
 {}
 Result:=QueueTableCount;
end;

{==============================================================================}

function ThreadGetCount:LongWord;
{Get the current thread count}
begin
 {}
 Result:=ThreadTableCount;
end;

{==============================================================================}

function ThreadTlsGetCount:LongWord;
{Get the current thread tls count}
begin
 {}
 Result:=ThreadTlsTableCount;
end;

{==============================================================================}

function ThreadAllocateStack(StackSize:LongWord):Pointer;
{Allocate memory for a new thread stack}
{StackSize: Number of bytes requested for new thread stack}
{Return: nil if StackSize was 0 or if there is not enough memory to satisfy the
         request
         Otherwise returns a pointer to the top (highest address) of the newly
         allocated memory region
         This address is the base of the stack which grows down in memory}
var
 StackMemory:Pointer;
 PageTableEntry:TPageTableEntry;
begin
 {}
 Result:=nil;

 {Check Stack Size}
 if StackSize = 0 then Exit;

 {Check Stack Guard}
 if THREAD_STACK_GUARD_ENABLED then
  begin
   {Check Page Size}
   if MEMORY_PAGE_SIZE = 0 then Exit;

   {Allocate Memory}
   StackMemory:=AllocAlignedMem(StackSize + MEMORY_PAGE_SIZE,STACK_MIN_ALIGNMENT);
   if StackMemory = nil then Exit;

   {Get Page}
   PageTableGetEntry(PtrUInt(StackMemory),PageTableEntry);
   if PageTableEntry.Size <> MEMORY_PAGE_SIZE then
    begin
     {Map Page}
     PageTableEntry.Size:=MEMORY_PAGE_SIZE;
     PageTableSetEntry(PageTableEntry);
    end;

   {Get Next Page}
   PageTableGetEntry(PtrUInt(StackMemory + MEMORY_PAGE_SIZE),PageTableEntry);
   if PageTableEntry.Size <> MEMORY_PAGE_SIZE then
    begin
     {Map Page}
     PageTableEntry.Size:=MEMORY_PAGE_SIZE;
     PageTableSetEntry(PageTableEntry);
    end;

   {Check Page}
   PageTableGetEntry(PtrUInt(StackMemory),PageTableEntry);
   if PageTableEntry.Size <> MEMORY_PAGE_SIZE then
    begin
     FreeMem(StackMemory);
     Exit;
    end;

   {Clean Guard Page}
   CleanDataCacheRange(PtrUInt(StackMemory),MEMORY_PAGE_SIZE);

   {Map Guard Page (No Access)}
   PageTableEntry.Flags:=PageTableEntry.Flags and not(PAGE_TABLE_FLAG_READONLY or PAGE_TABLE_FLAG_READWRITE or PAGE_TABLE_FLAG_CACHEABLE or PAGE_TABLE_FLAG_WRITEBACK or PAGE_TABLE_FLAG_WRITETHROUGH or PAGE_TABLE_FLAG_WRITEALLOCATE);
   PageTableSetEntry(PageTableEntry);

   {Return Top Address}
   Result:=StackMemory + (StackSize + MEMORY_PAGE_SIZE);
  end
 else
  begin
   {Allocate Memory}
   StackMemory:=AllocAlignedMem(StackSize,STACK_MIN_ALIGNMENT);
   if StackMemory = nil then Exit;

   {Return Top Address}
   Result:=StackMemory + StackSize;
  end;
end;

{==============================================================================}

procedure ThreadReleaseStack(StackBase:Pointer;StackSize:LongWord);
{Release a thread stack allocated with ThreadAllocateStack}
{StackBase: Pointer to the top (highest address) of the thread stack
            (as returned by ThreadAllocateStack}
{StackSize: Size of the thread stack, in bytes (Same value passed to ThreadAllocateStack)}
var
 StackMemory:Pointer;
 PageTableEntry:TPageTableEntry;
 GuardPageEntry:TPageTableEntry;
begin
 {}
 {Check Stack Address}
 if StackBase = nil then Exit;

 {Check Stack Size}
 if StackSize = 0 then Exit;

 {Check Stack Guard}
 if THREAD_STACK_GUARD_ENABLED then
  begin
   {Get Base Address}
   StackMemory:=StackBase - (StackSize + MEMORY_PAGE_SIZE);

   {Get Next Page}
   PageTableGetEntry(PtrUInt(StackMemory + MEMORY_PAGE_SIZE),PageTableEntry);

   {Get Guard Page}
   PageTableGetEntry(PtrUInt(StackMemory),GuardPageEntry);

   {Unmap Guard Page (Normal Access)}
   GuardPageEntry.Flags:=PageTableEntry.Flags;
   PageTableSetEntry(GuardPageEntry);

   {Free Memory}
   FreeMem(StackMemory,StackSize + MEMORY_PAGE_SIZE);
  end
 else
  begin
   {Get Base Address}
   StackMemory:=StackBase - StackSize;

   {Free Memory}
   FreeMem(StackMemory,StackSize);
  end;
end;

{==============================================================================}

function ThreadSetupStack(StackBase:Pointer;StartProc:TThreadStart;ReturnProc:TThreadEnd;Parameter:Pointer):Pointer;
begin
 {}
 if Assigned(ThreadSetupStackHandler) then
  begin
   Result:=ThreadSetupStackHandler(StackBase,StartProc,ReturnProc,Parameter);
  end
 else
  begin
   Result:=nil;
  end;
end;

{==============================================================================}

function ThreadSnapshotCreate:PThreadSnapshot;
var
 Current:PThreadSnapshot;
 Previous:PThreadSnapshot;
 Snapshot:PThreadSnapshot;
 ThreadEntry:PThreadEntry;
begin
 {}
 Result:=nil;

 {Acquire Lock}
 if SpinLock(ThreadTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Count}
    if ThreadTableCount = 0 then Exit;

    {Allocate Snapshot}
    Snapshot:=AllocMem(SizeOf(TThreadSnapshot) * ThreadTableCount);
    if Snapshot = nil then Exit;

    {Setup Start}
    Current:=Snapshot;
    Previous:=nil;

    {Get First Thread}
    ThreadEntry:=ThreadTable;
    while ThreadEntry <> nil do
     begin
      {Add Thread}
      Current.Handle:=TThreadHandle(ThreadEntry);
      Current.State:=ThreadEntry.State;
      Current.Flags:=ThreadEntry.Flags;
      Current.CPU:=ThreadEntry.CurrentCPU;
      Current.Priority:=ThreadEntry.Priority;
      Current.Affinity:=ThreadEntry.Affinity;
      Current.StackBase:=ThreadEntry.StackBase;
      Current.StackSize:=ThreadEntry.StackSize;
      Current.StackPointer:=ThreadEntry.StackPointer;
      StrLCopy(Current.Name,ThreadEntry.Name,THREAD_NAME_LENGTH - 1);
      Current.Parent:=ThreadEntry.Parent;
      Current.ExitCode:=ThreadEntry.ExitCode;
      Current.LastError:=ThreadEntry.LastError;
      Current.Locale:=ThreadEntry.Locale;
      Current.TargetCPU:=ThreadEntry.TargetCPU;
      Current.TargetPriority:=ThreadEntry.TargetPriority;
      Current.CreateTime:=ThreadEntry.CreateTime;
      Current.ExitTime:=ThreadEntry.ExitTime;
      Current.KernelTime:=ThreadEntry.KernelTime;
      Current.SwitchCount:=ThreadEntry.SwitchCount;

      {Add Next}
      if Previous <> nil then Previous.Next:=Current;
      Previous:=Current;
      Current:=PThreadSnapshot(PtrUInt(Previous) + SizeOf(TThreadSnapshot));

      {Get Next Thread}
      ThreadEntry:=ThreadEntry.Next;
     end;

    {Return Result}
    Result:=Snapshot;
   finally
    SpinUnlock(ThreadTableLock);
   end;
  end;
end;

{==============================================================================}

function ThreadSnapshotDestroy(ASnapshot:PThreadSnapshot):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Snapshot}
 if ASnapshot = nil then Exit;

 {Free Snapshot}
 FreeMem(ASnapshot);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function MessageslotGetCount:LongWord;
{Get the current messageslot count}
begin
 {}
 Result:=MessageslotTableCount;
end;

{==============================================================================}

function MailslotGetCount:LongWord;
{Get the current mailslot count}
begin
 {}
 Result:=MailslotTableCount;
end;

{==============================================================================}

function BufferGetCount:LongWord;
{Get the current buffer count}
begin
 {}
 Result:=BufferTableCount;
end;

{==============================================================================}

function EventGetCount:LongWord;
{Get the current event count}
begin
 {}
 Result:=EventTableCount;
end;

{==============================================================================}

function TimerGetCount:LongWord;
{Get the current timer count}
begin
 {}
 Result:=TimerTableCount;
end;

{==============================================================================}

function WorkerGetCount:LongWord;
{Get the current worker thread count}
begin
 {}
 Result:=WorkerThreadCount;
end;

{==============================================================================}

function WorkerGetPriorityCount:LongWord;
{Get the current worker priority thread count}
begin
 {}
 Result:=WorkerPriorityThreadCount;
end;

{==============================================================================}

function TaskerGetCount:LongWord;
{Get the current tasker count}
begin
 {}
 Result:=0;

 {Check List}
 if TaskerList = nil then Exit;

 Result:=TaskerList.Count;
end;

{==============================================================================}

function ListTypeToString(ListType:LongWord):String;
begin
 {}
 Result:='';

 case ListType of
  LIST_TYPE_NOT_SPECIFIED:Result:='LIST_TYPE_NOT_SPECIFIED';
  LIST_TYPE_WAIT_SECTION:Result:='LIST_TYPE_WAIT_SECTION';
  LIST_TYPE_WAIT_SEMAPHORE:Result:='LIST_TYPE_WAIT_SEMAPHORE';
  LIST_TYPE_WAIT_SYNCHRONIZER:Result:='LIST_TYPE_WAIT_SYNCHRONIZER';
  LIST_TYPE_WAIT_CONDITION:Result:='LIST_TYPE_WAIT_CONDITION';
  LIST_TYPE_WAIT_COMPLETION:Result:='LIST_TYPE_WAIT_COMPLETION';
  LIST_TYPE_WAIT_EVENT:Result:='LIST_TYPE_WAIT_EVENT';
  LIST_TYPE_WAIT_THREAD:Result:='LIST_TYPE_WAIT_THREAD';
  LIST_TYPE_WAIT_MESSAGESLOT:Result:='LIST_TYPE_WAIT_MESSAGESLOT';
  LIST_TYPE_WAIT_OTHER:Result:='LIST_TYPE_WAIT_OTHER';
 end;
end;

{==============================================================================}

function QueueTypeToString(QueueType:LongWord):String;
begin
 {}
 Result:='';

 case QueueType of
  QUEUE_TYPE_NOT_SPECIFIED:Result:='QUEUE_TYPE_NOT_SPECIFIED';
  QUEUE_TYPE_SCHEDULE_SLEEP:Result:='QUEUE_TYPE_SCHEDULE_SLEEP';
  QUEUE_TYPE_SCHEDULE_TIMEOUT:Result:='QUEUE_TYPE_SCHEDULE_TIMEOUT';
  QUEUE_TYPE_SCHEDULE_TERMINATION:Result:='QUEUE_TYPE_SCHEDULE_TERMINATION';
  QUEUE_TYPE_SCHEDULE_NONE:Result:='QUEUE_TYPE_SCHEDULE_NONE';
  QUEUE_TYPE_SCHEDULE_IDLE:Result:='QUEUE_TYPE_SCHEDULE_IDLE';
  QUEUE_TYPE_SCHEDULE_LOWEST:Result:='QUEUE_TYPE_SCHEDULE_LOWEST';
  QUEUE_TYPE_SCHEDULE_LOWER:Result:='QUEUE_TYPE_SCHEDULE_LOWER';
  QUEUE_TYPE_SCHEDULE_NORMAL:Result:='QUEUE_TYPE_SCHEDULE_NORMAL';
  QUEUE_TYPE_SCHEDULE_HIGHER:Result:='QUEUE_TYPE_SCHEDULE_HIGHER';
  QUEUE_TYPE_SCHEDULE_HIGHEST:Result:='QUEUE_TYPE_SCHEDULE_HIGHEST';
  QUEUE_TYPE_SCHEDULE_CRITICAL:Result:='QUEUE_TYPE_SCHEDULE_CRITICAL';
 end;
end;

{==============================================================================}

function ThreadTypeToString(ThreadType:LongWord):String;
begin
 {}
 Result:='';

 case ThreadType of
  THREAD_TYPE_NORMAL:Result:='THREAD_TYPE_NORMAL';
  THREAD_TYPE_IDLE:Result:='THREAD_TYPE_IDLE';
  THREAD_TYPE_IRQ:Result:='THREAD_TYPE_IRQ';
  THREAD_TYPE_FIQ:Result:='THREAD_TYPE_FIQ';
  THREAD_TYPE_SWI:Result:='THREAD_TYPE_SWI';
 end;
end;

{==============================================================================}

function ThreadStateToString(ThreadState:LongWord):String;
begin
 {}
 Result:='';

 case ThreadState of
  THREAD_STATE_RUNNING:Result:='THREAD_STATE_RUNNING';
  THREAD_STATE_READY:Result:='THREAD_STATE_READY';
  THREAD_STATE_SLEEP:Result:='THREAD_STATE_SLEEP';
  THREAD_STATE_SUSPENDED:Result:='THREAD_STATE_SUSPENDED';
  THREAD_STATE_WAIT:Result:='THREAD_STATE_WAIT';
  THREAD_STATE_WAIT_TIMEOUT:Result:='THREAD_STATE_WAIT_TIMEOUT';
  THREAD_STATE_RECEIVE:Result:='THREAD_STATE_RECEIVE';
  THREAD_STATE_RECEIVE_TIMEOUT:Result:='THREAD_STATE_RECEIVE_TIMEOUT';
  THREAD_STATE_HALTED:Result:='THREAD_STATE_HALTED';
  THREAD_STATE_TERMINATED:Result:='THREAD_STATE_TERMINATED';
 end;
end;

{==============================================================================}

function ThreadPriorityToString(ThreadPriority:LongWord):String;
begin
 {}
 Result:='';

 case ThreadPriority of
  THREAD_PRIORITY_NONE:Result:='THREAD_PRIORITY_NONE';
  THREAD_PRIORITY_IDLE:Result:='THREAD_PRIORITY_IDLE';
  THREAD_PRIORITY_LOWEST:Result:='THREAD_PRIORITY_LOWEST';
  THREAD_PRIORITY_LOWER:Result:='THREAD_PRIORITY_LOWER';
  THREAD_PRIORITY_NORMAL:Result:='THREAD_PRIORITY_NORMAL';
  THREAD_PRIORITY_HIGHER:Result:='THREAD_PRIORITY_HIGHER';
  THREAD_PRIORITY_HIGHEST:Result:='THREAD_PRIORITY_HIGHEST';
  THREAD_PRIORITY_CRITICAL:Result:='THREAD_PRIORITY_CRITICAL';
 end;
end;

{==============================================================================}

procedure ThreadLog(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < THREAD_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = THREAD_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = THREAD_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = THREAD_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Threads: ';

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_THREADS,LogLevelToLoggingSeverity(Level),'Threads',WorkBuffer + AText);
end;

{==============================================================================}

procedure ThreadLogInfo(const AText:String); inline;
begin
 {}
 ThreadLog(THREAD_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure ThreadLogWarn(const AText:String); inline;
begin
 {}
 ThreadLog(THREAD_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure ThreadLogError(const AText:String); inline;
begin
 {}
 ThreadLog(THREAD_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure ThreadLogDebug(const AText:String); inline;
begin
 {}
 ThreadLog(THREAD_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}
{Scheduler Helper Functions}
function SchedulerGetListFlags(ListType:LongWord):LongWord;
{Get the list flags for the specified type of list}
var
 Flags:LongWord;
begin
 {}
 Result:=LIST_FLAG_NONE;

 {Get IRQ/FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   Flags:=LIST_FLAG_IRQFIQ;
  end
 else
  begin
   Flags:=LIST_FLAG_IRQ;
  end;

 {Check List Type}
 case ListType of
  LIST_TYPE_WAIT_SECTION:Result:=Flags;
  LIST_TYPE_WAIT_SEMAPHORE:Result:=Flags;
  LIST_TYPE_WAIT_SYNCHRONIZER:Result:=Flags;
  LIST_TYPE_WAIT_CONDITION:Result:=Flags;
  LIST_TYPE_WAIT_COMPLETION:Result:=Flags;
  LIST_TYPE_WAIT_EVENT:Result:=Flags;
  LIST_TYPE_WAIT_THREAD:Result:=Flags;
  LIST_TYPE_WAIT_MESSAGESLOT:Result:=Flags;
  LIST_TYPE_WAIT_OTHER:Result:=Flags;  {Other type of wait list requires the same behaviour as all synchronisation objects}
 end;
end;

{==============================================================================}

function SchedulerGetQueueFlags(QueueType:LongWord):LongWord;
{Get the queue flags for the specified type of scheduler queue}
var
 Flags:LongWord;
begin
 {}
 Result:=QUEUE_FLAG_NONE;

 {Get IRQ/FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   Flags:=QUEUE_FLAG_IRQFIQ;
  end
 else
  begin
   Flags:=QUEUE_FLAG_IRQ;
  end;

 {Check Queue Type}
 case QueueType of
  QUEUE_TYPE_SCHEDULE_SLEEP:Result:=Flags or QUEUE_FLAG_DELTA;
  QUEUE_TYPE_SCHEDULE_TIMEOUT:Result:=Flags or QUEUE_FLAG_DELTA;
  QUEUE_TYPE_SCHEDULE_TERMINATION:Result:=Flags or QUEUE_FLAG_DELTA;
  QUEUE_TYPE_SCHEDULE_NONE:Result:=Flags or QUEUE_FLAG_DESCENDING;
  QUEUE_TYPE_SCHEDULE_IDLE:Result:=Flags or QUEUE_FLAG_DESCENDING;
  QUEUE_TYPE_SCHEDULE_LOWEST:Result:=Flags or QUEUE_FLAG_DESCENDING;
  QUEUE_TYPE_SCHEDULE_LOWER:Result:=Flags or QUEUE_FLAG_DESCENDING;
  QUEUE_TYPE_SCHEDULE_NORMAL:Result:=Flags or QUEUE_FLAG_DESCENDING;
  QUEUE_TYPE_SCHEDULE_HIGHER:Result:=Flags or QUEUE_FLAG_DESCENDING;
  QUEUE_TYPE_SCHEDULE_HIGHEST:Result:=Flags or QUEUE_FLAG_DESCENDING;
  QUEUE_TYPE_SCHEDULE_CRITICAL:Result:=Flags or QUEUE_FLAG_DESCENDING;
 end;
end;

{==============================================================================}

function SchedulerGetQueueHandle(CPUID:LongWord;QueueType:LongWord):TQueueHandle;
{Get the queue handle for the specified type of scheduler queue on the specified CPU}
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Queue Type}
 case QueueType of
  QUEUE_TYPE_SCHEDULE_SLEEP:Result:=SchedulerSleepQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_TIMEOUT:Result:=SchedulerTimeoutQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_TERMINATION:Result:=SchedulerTerminationQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_NONE:Result:=SchedulerNoneQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_IDLE:Result:=SchedulerIdleQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_LOWEST:Result:=SchedulerLowestQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_LOWER:Result:=SchedulerLowerQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_NORMAL:Result:=SchedulerNormalQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_HIGHER:Result:=SchedulerHigherQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_HIGHEST:Result:=SchedulerHighestQueue[CPUID];
  QUEUE_TYPE_SCHEDULE_CRITICAL:Result:=SchedulerCriticalQueue[CPUID];
 end;
end;

{==============================================================================}

function SchedulerGetQueueHandleEx(CPUID:LongWord;Priority:LongWord):TQueueHandle;
{Get the queue handle for the specified thread priority on the specified CPU}
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Priority Type}
 case Priority of
  THREAD_PRIORITY_NONE:Result:=SchedulerNoneQueue[CPUID];
  THREAD_PRIORITY_IDLE:Result:=SchedulerIdleQueue[CPUID];
  THREAD_PRIORITY_LOWEST:Result:=SchedulerLowestQueue[CPUID];
  THREAD_PRIORITY_LOWER:Result:=SchedulerLowerQueue[CPUID];
  THREAD_PRIORITY_NORMAL:Result:=SchedulerNormalQueue[CPUID];
  THREAD_PRIORITY_HIGHER:Result:=SchedulerHigherQueue[CPUID];
  THREAD_PRIORITY_HIGHEST:Result:=SchedulerHighestQueue[CPUID];
  THREAD_PRIORITY_CRITICAL:Result:=SchedulerCriticalQueue[CPUID];
 end;
end;

{==============================================================================}

function SchedulerGetThreadCount(CPUID:LongWord):LongWord;
{Get the thread count for the specified CPU}
 begin
 {}
 Result:=0;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Get Thread Count}
 Result:=SchedulerThreadCount[CPUID];
end;

{==============================================================================}

function SchedulerGetThreadQuantum(CPUID:LongWord):LongWord;
{Get the current thread quantum for the specified CPU}
begin
 {}
 Result:=0;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Get Thread Quantum}
 Result:=SchedulerThreadQuantum[CPUID];
end;

{==============================================================================}

function SchedulerGetThreadHandle(CPUID:LongWord;ThreadType:LongWord):TThreadHandle;
{Get the thread handle for the specified type of thread on the specified CPU}
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Check Thread Type}
 case ThreadType of
  THREAD_TYPE_IDLE:Result:=IDLE_THREAD_HANDLE[CPUID];
  THREAD_TYPE_IRQ:Result:=IRQ_THREAD_HANDLE[CPUID];
  THREAD_TYPE_FIQ:Result:=FIQ_THREAD_HANDLE[CPUID];
  THREAD_TYPE_SWI:Result:=SWI_THREAD_HANDLE[CPUID];
 end;
end;

{==============================================================================}

function SchedulerGetPriorityMask(CPUID:LongWord):LongWord;
{Get the current priority mask for the specified CPU}
begin
 {}
 Result:=0;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Get Priority Mask}
 Result:=SchedulerPriorityMask[CPUID];
end;

{==============================================================================}

function SchedulerGetPriorityQuantum(Priority:LongWord):LongWord;
{Get the scheduler quantum for the specified thread priority}
begin
 {}
 Result:=LongWord(INVALID_HANDLE_VALUE);

 {Check Priority}
 if Priority > THREAD_PRIORITY_CRITICAL then Exit;

 {Get Priority Quantum}
 Result:=SCHEDULER_PRIORITY_QUANTUM[Priority];
end;

{==============================================================================}

function SchedulerSetPriorityQuantum(Priority,Quantum:LongWord):LongWord;
{Set the scheduler quantum for the specified thread priority}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Priority}
 if Priority > THREAD_PRIORITY_CRITICAL then Exit;

 {Set Priority Quantum}
 SCHEDULER_PRIORITY_QUANTUM[Priority]:=Quantum;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SchedulerGetMigrationQuantum:LongWord;
{Get the current migration quantum}
begin
 {}
 Result:=SchedulerMigrationQuantum;
end;

{==============================================================================}

function SchedulerGetStarvationQuantum(CPUID:LongWord):LongWord;
{Get the current starvation quantum for the specified CPU}
begin
 {}
 Result:=0;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Get Starvation Quantum}
 Result:=SchedulerStarvationQuantum[CPUID];
end;

{==============================================================================}

function SchedulerGetThreadNext:LongWord;
{Get the next CPU for thread allocation}
begin
 {}
 Result:=SchedulerThreadNext;
end;

{==============================================================================}

function SchedulerGetThreadMigration:LongWord;
{Get the current thread migration setting}
begin
 {}
 Result:=SchedulerThreadMigration;
end;

{==============================================================================}

function SchedulerGetThreadPreempt(CPUID:LongWord):LongWord;
{Get the current thread preempt setting for the specified CPU}
begin
 {}
 Result:=SCHEDULER_PREEMPT_DISABLED;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Get Thread Preempt}
 Result:=SchedulerThreadPreempt[CPUID];
end;

{==============================================================================}

function SchedulerGetThreadAllocation(CPUID:LongWord):LongWord;
{Get the current thread allocation setting for the specified CPU}
begin
 {}
 Result:=SCHEDULER_ALLOCATION_DISABLED;

 {Check CPU}
 if CPUID > (SCHEDULER_CPU_COUNT - 1) then Exit;

 {Get Thread Allocation}
 Result:=SchedulerThreadAllocation[CPUID];
end;

{==============================================================================}

function SchedulerMigrationToString(Migration:LongWord):String;
begin
 {}
 Result:='';

 case Migration of
  SCHEDULER_MIGRATION_DISABLED:Result:='SCHEDULER_MIGRATION_DISABLED';
  SCHEDULER_MIGRATION_ENABLED:Result:='SCHEDULER_MIGRATION_ENABLED';
 end;
end;

{==============================================================================}

function SchedulerPreemptToString(Preempt:LongWord):String;
begin
 {}
 Result:='';

 case Preempt of
  SCHEDULER_PREEMPT_DISABLED:Result:='SCHEDULER_PREEMPT_DISABLED';
  SCHEDULER_PREEMPT_ENABLED:Result:='SCHEDULER_PREEMPT_ENABLED';
 end;
end;

{==============================================================================}

function SchedulerAllocationToString(Allocation:LongWord):String;
begin
 {}
 Result:='';

 case Allocation of
  SCHEDULER_ALLOCATION_DISABLED:Result:='SCHEDULER_ALLOCATION_DISABLED';
  SCHEDULER_ALLOCATION_ENABLED:Result:='SCHEDULER_ALLOCATION_ENABLED';
 end;
end;

{==============================================================================}
{==============================================================================}
{Timer Helper Functions}
function TimerGetListFlags:LongWord;
{Get the lock flags for the timer list}
begin
 {}
 {Get IRQ/FIQ}
 if CLOCK_FIQ_ENABLED then
  begin
   Result:=LOCK_FLAG_IRQFIQ;
  end
 else
  begin
   Result:=LOCK_FLAG_IRQ;
  end;
end;

{==============================================================================}

function TimerGetMessageslotFlags:LongWord;
{Get the lock flags for the timer messageslot}
begin
 {}
 {Get IRQ/FIQ}
 if CLOCK_FIQ_ENABLED then
  begin
   Result:=LOCK_FLAG_IRQFIQ;
  end
 else
  begin
   Result:=LOCK_FLAG_IRQ;
  end;
end;

{==============================================================================}
{==============================================================================}
{Worker Helper Functions}
function WorkerGetMessageslotFlags:LongWord;
{Get the lock flags for the worker messageslot}
begin
 {}
 {Get IRQ/FIQ}
 if SCHEDULER_FIQ_ENABLED then
  begin
   Result:=LOCK_FLAG_IRQFIQ;
  end
 else
  begin
   Result:=LOCK_FLAG_IRQ;
  end;
end;

{==============================================================================}
{==============================================================================}
{Tasker Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 {Call SetThreadManager again because initialization of the system unit (InitSystemThreads) sets the NoThreadManager}
 SetThreadManager(MyThreadManager);
 {Set the Main Thread ID again because initialization of the system unit (InitSystemThreads) sets it to ThreadID(1)}
 ThreadID:=ThreadGetCurrent;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

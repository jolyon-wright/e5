#include "driver.h"

static VOID
SimpleFilterForwardRequestUnmodified(__in WDFDEVICE  Device,
                                     __in WDFREQUEST Request
                                     );

#ifdef ALLOC_PRAGMA
#pragma alloc_text (PAGE, simplefilterQueueInitialize)
#endif


VOID
SimpleFilterForwardRequestUnmodified(__in WDFDEVICE  Device,
                                     __in WDFREQUEST Request
                                     )
{
	WDF_REQUEST_SEND_OPTIONS options;
    NTSTATUS                 status;

    //
	// We're just going to be passing this request on with
	//  zero regard for what happens to it. Therefore, we'll
	//  use the WDF_REQUEST_SEND_OPTION_SEND_AND_FORGET option
	//
    WDF_REQUEST_SEND_OPTIONS_INIT(&options,
                                  WDF_REQUEST_SEND_OPTION_SEND_AND_FORGET
                                  );


	//
	// And send it!
	//
	if (!WdfRequestSend(Request,
                        WdfDeviceGetIoTarget(Device),
                        &options
                        )
        ) {

		//
		// Oops! Something bad happened, complete the request
		//
        status = WdfRequestGetStatus(Request);
        WdfRequestComplete(Request, status);
    }

	return;
}

NTSTATUS
simplefilterQueueInitialize(
    __in WDFDEVICE Device
    )
{
    WDFQUEUE            queue;
    NTSTATUS            status;
    WDF_IO_QUEUE_CONFIG queueConfig;

    PAGED_CODE();

    WDF_IO_QUEUE_CONFIG_INIT_DEFAULT_QUEUE(&queueConfig,
                                           WdfIoQueueDispatchParallel
                                           );

    queueConfig.EvtIoInternalDeviceControl = simplefilterEvtIoInternalDeviceControl;
    queueConfig.EvtIoStop                  = simplefilterEvtIoStop;

    status = WdfIoQueueCreate(Device,
                              &queueConfig,
                              WDF_NO_OBJECT_ATTRIBUTES,
                              &queue
                              );

    if(!NT_SUCCESS(status)) {
        return status;
    }

    return status;
}

VOID
simplefilterEvtIoInternalDeviceControl(__in WDFQUEUE   Queue,
                                       __in WDFREQUEST Request,
                                       __in size_t     OutputBufferLength,
                                       __in size_t     InputBufferLength,
                                       __in ULONG      IoControlCode
                                       )
{
    //WdfRequestForwardToIoQueue(Request, Queue);
    KdPrint(("%s() called\n", __FUNCTION__));

    SimpleFilterForwardRequestUnmodified(WdfIoQueueGetDevice(Queue),
                                         Request
                                         );


    return;
}

VOID
simplefilterEvtIoStop(__in WDFQUEUE   Queue,
                      __in WDFREQUEST Request,
                      __in ULONG      ActionFlags
                      )
{
    
}

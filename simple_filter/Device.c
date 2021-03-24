
// how to install it:-

/* copy simple_filter.sys c:\windows\system32\drivers */
/* sc create simple_filter binpath= %windir%\system32\drivers\simple_filter.sys type= kernel start= demand */

#include "driver.h"

#ifdef ALLOC_PRAGMA
#pragma alloc_text (PAGE, simplefilterCreateDevice)
#endif

NTSTATUS
simplefilterCreateDevice(
    __inout PWDFDEVICE_INIT DeviceInit
    )
{
    WDF_OBJECT_ATTRIBUTES deviceAttributes;
    PDEVICE_CONTEXT       deviceContext;
    WDFDEVICE             device;
    NTSTATUS              status;

    PAGED_CODE();

    WDF_OBJECT_ATTRIBUTES_INIT_CONTEXT_TYPE(&deviceAttributes, DEVICE_CONTEXT);

    WdfFdoInitSetFilter(DeviceInit);
    WdfDeviceInitSetDeviceType(DeviceInit,
                               FILE_DEVICE_UNKNOWN
                               );

    status = WdfDeviceCreate(&DeviceInit, &deviceAttributes, &device);

    if (NT_SUCCESS(status)) {
        deviceContext = DeviceGetContext(device); // Device.h
        deviceContext->PrivateDeviceData = 0;

        status = WdfDeviceCreateDeviceInterface(device,
                                                &GUID_DEVINTERFACE_simplefilter, // Public.h
                                                NULL // ReferenceString
                                                );

        if (NT_SUCCESS(status)) {
            status = simplefilterQueueInitialize(device);
        }
    }

    return status;
}

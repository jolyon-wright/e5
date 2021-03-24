#include "driver.h"

#ifdef ALLOC_PRAGMA
#pragma alloc_text (INIT, DriverEntry)
#pragma alloc_text (PAGE, simplefilterEvtDeviceAdd)
#pragma alloc_text (PAGE, simplefilterEvtDriverContextCleanup)
#endif

NTSTATUS
DriverEntry(
    __in PDRIVER_OBJECT  DriverObject,
    __in PUNICODE_STRING RegistryPath
    )
{
    WDF_DRIVER_CONFIG config;
    NTSTATUS status;
    WDF_OBJECT_ATTRIBUTES attributes;

    __debugbreak();

    WDF_OBJECT_ATTRIBUTES_INIT(&attributes);
    attributes.EvtCleanupCallback = simplefilterEvtDriverContextCleanup;

    WDF_DRIVER_CONFIG_INIT(&config,
                           simplefilterEvtDeviceAdd
                           );

    status = WdfDriverCreate(DriverObject,
                             RegistryPath,
                             &attributes,
                             &config,
                             WDF_NO_HANDLE
                             );

    if (!NT_SUCCESS(status)) {
        return status;
    }

    return status;
}

NTSTATUS
simplefilterEvtDeviceAdd(
    __in    WDFDRIVER       Driver,
    __inout PWDFDEVICE_INIT DeviceInit
    )
{
    NTSTATUS status;

    UNREFERENCED_PARAMETER(Driver);

    PAGED_CODE();

    status = simplefilterCreateDevice(DeviceInit);

    return status;
}

VOID
simplefilterEvtDriverContextCleanup(
    __in WDFOBJECT DriverObject
    )
{
    UNREFERENCED_PARAMETER(DriverObject);

    PAGED_CODE();
}

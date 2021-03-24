/*++

Module Name:

    public.h

Abstract:

    This module contains the common declarations shared by driver
    and user applications.

Environment:

    user and kernel

--*/

//
// Define an Interface Guid so that apps can find the device and talk to it.
//

DEFINE_GUID (GUID_DEVINTERFACE_simplefilter,
    0xecb49aef,0x29ad,0x4d8c,0x98,0xe7,0x26,0x9d,0x1f,0xef,0xf1,0x6f);
// {ecb49aef-29ad-4d8c-98e7-269d1feff16f}

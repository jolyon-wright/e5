_LNG=$(LANGUAGE)
_INX=.
STAMP=stampinf -f $@ -a $(_BUILDARCH) -k $(KMDF_VERSION_MAJOR).$(KMDF_VERSION_MINOR) -v $(BUSDOG_VERSION)



$(OBJ_PATH)\$(O)\$(INF_NAME).inf: $(_INX)\$(INF_NAME).inx 
    copy $(_INX)\$(@B).inx $@
    $(STAMP)


!ifndef PRIVATE_SYMBOL_STORE
!message "******************************************** jolyon says : PRIVATE_SYMBOL_STORE note defined - using \\JOLYONPC\dev\driver_symbols. Please define locally!"
PRIVATE_SYMBOL_STORE=\\JOLYONPC\dev\driver_symbols
!else
!message using symbol store : $(PRIVATE_SYMBOL_STORE)
!endif

postbuild:
!ifdef I_AM_HASSAN
	   signtool sign /v /ac "v:\dev\client\drivers\signing\microsoft\MSCV-VSClass3.cer" /s MY /n "Ecrebo Limited" /t http://timestamp.VeriSign.com/scripts/timstamp.dll $(TARGET)
!endif
!ifdef TEST_BOX
    md $(TEST_BOX)\$(O)
    copy $(TARGET) $(TEST_BOX)\$(O)
    copy $(O)\$(TARGETNAME).pdb $(TEST_BOX)\$(O)
!endif
    symstore add /r /f $(O)\*.* /s $(PRIVATE_SYMBOL_STORE) /t "ecrebo driver" /v "unversioned" /c "an add"

        IF LOCALE_CHSET = LOCAL_CHSET_JP
                        db      "JP"
        ENDIF
        IF LOCALE_CHSET = LOCAL_CHSET_US
                IF LOCALE_CHSET_VAR = LOCAL_CHSET_VAR_NONE
                        db      "EU/INT"
                ELSE
                IF LOCALE_CHSET_VAR = LOCAL_CHSET_VAR_BR
                        db      "BR"
                ENDIF
                ENDIF
        ENDIF

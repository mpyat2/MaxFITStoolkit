
{$IFOPT R+}{$DEFINE range_check}{$ENDIF}
{$IFOPT Q+}{$DEFINE overflow_check}{$ENDIF}

{.DEFINE ROUND_C}  // if enabled, rounding occurs like in C programs
                   // if disabled, FPC uses "banker's" rounding (could be better)



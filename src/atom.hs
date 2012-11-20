import Language.Atom

blink :: Atom ()
blink = do
  on <- bool "on" True
  period ph $ phase 0 $ atom "blinkOn" $ do
    call "avr_blink"
    on <== not_ (value on)
  period ph $ phase (quot ph 100) $ atom "blinkOff" $ do
    call "avr_blink"
    on <== not_ (value on)
  where
    ph = 680000

main :: IO ()
main = do
  (sch, _, _, _, _) <- compile atomName defaults {cCode = prePostCode} blink
  putStrLn $ reportSchedule sch
  where
    atomName = "blink"
    varInit t k v = cType t ++ " " ++ k ++ " = " ++ v ++ ";"
    prePostCode _ _ _ =
      ( unlines
        [ "#include <Arduino.h>"
        , varInit Int16 "ledPin" "13"
        , "void avr_blink(void);"
        ]
      , unlines
        [ "void setup() { pinMode(ledPin, OUTPUT); }"
        , "void avr_blink() {"
        , "  digitalWrite(ledPin, state." ++ atomName ++ ".on);"
        , "}"
        , "void loop() { " ++ atomName ++ "(); }"
        ]
      )

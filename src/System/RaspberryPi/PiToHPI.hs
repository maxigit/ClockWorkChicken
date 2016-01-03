-- | This module implements the Pi interface using the HPi package
module System.RaspberryPi.PiToHPI (
System.RaspberryPi.PiToHPI.pi
) where

import System.RaspberryPi as P
import System.RaspberryPi.GPIO as HPI

pinToHPiPin :: P.Pin -> HPI.Pin
pinToHPiPin P.Pin3 = HPI.Pin03
-- pinToHPiPin P.Pin04 = HPI.Pin04
pinToHPiPin P.Pin5 = HPI.Pin05
-- pinToHPiPin P.Pin06 = HPI.Pin06
pinToHPiPin P.Pin7 = HPI.Pin07
pinToHPiPin P.Pin8 = HPI.Pin08
pinToHPiPin P.Pin10 = HPI.Pin10
pinToHPiPin P.Pin11 = HPI.Pin11
pinToHPiPin P.Pin12 = HPI.Pin12
pinToHPiPin P.Pin13 = HPI.Pin13
-- pinToHPiPin P.Pin14 = HPI.Pin14
pinToHPiPin P.Pin15 = HPI.Pin15
pinToHPiPin P.Pin16 = HPI.Pin16
pinToHPiPin P.Pin18 = HPI.Pin18
pinToHPiPin P.Pin19 = HPI.Pin19
-- pinToHPiPin P.Pin20 = HPI.Pin20
pinToHPiPin P.Pin21 = HPI.Pin21
pinToHPiPin P.Pin22 = HPI.Pin22
pinToHPiPin P.Pin23 = HPI.Pin23
pinToHPiPin P.Pin24 = HPI.Pin24
-- pinToHPiPin P.Pin25 = HPI.Pin25
pinToHPiPin P.Pin26 = HPI.Pin26


levelToHPI :: P.Level -> HPI.LogicLevel
levelToHPI P.Low = False
levelToHPI P.High = True

hpiToLevel False = P.Low
hpiToLevel True = P.High


pi :: e -> Pi IO e
pi e = Pi read write e run
  where
    read = fmap hpiToLevel . HPI.readPin . pinToHPiPin
    write pin level = HPI.writePin  (pinToHPiPin pin) (levelToHPI level)
    run = HPI.withGPIO

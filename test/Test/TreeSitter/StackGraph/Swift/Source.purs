module Test.TreeSitter.StackGraph.Swift.Source where

import Prelude


data FileSystem a
    = File String a
    | Dir String (Array (FileSystem a))

derive instance Functor FileSystem

projectTennis :: FileSystem String
projectTennis = Dir "Swift" []

projectParrot :: FileSystem String
projectParrot = Dir "Swift"
    [ Dir "Parrot"
        [ File "Parrot.swift" """\
import Foundation

class Parrot {
    private let parrotType: ParrotTypeEnum
    private let numberOfCoconuts: Int
    private let voltage: Double
    private let isNailed: Bool

    init(_ parrotType: ParrotTypeEnum, numberOfCoconuts: Int, voltage: Double, isNailed: Bool) {
        self.parrotType = parrotType
        self.numberOfCoconuts = numberOfCoconuts
        self.voltage = voltage
        self.isNailed = isNailed
    }

    var speed: Double {
        switch parrotType {
        case .european:
            return baseSpeed

        case .african:
            return max(0, baseSpeed - loadFactor * Double(numberOfCoconuts));

        case .norwegianBlue:
            return (isNailed) ? 0 : baseSpeed(voltage: voltage)
        }
    }

    private func baseSpeed(voltage: Double) -> Double {
        return min(24.0, voltage*baseSpeed)
    }

    private var loadFactor: Double {
        return 9.0
    }

    private var baseSpeed: Double {
        return 12.0
    }
}
"""
        , File "ParrotTypeEnum.swift" """\
import Foundation

enum ParrotTypeEnum {
    case european, african, norwegianBlue
}
"""
        ]
    , Dir "ParrotTests"
        [ File "ParrotTests.swift" """\
import XCTest

class ParrotTests: XCTestCase {
    func testSpeedOfEuropeanParrot() {
        let parrot = Parrot(.european, numberOfCoconuts: 0, voltage: 0.0, isNailed: false);
        XCTAssertEqual(parrot.speed, 12.0)
    }

    func testSpeedOfAfricanParrot_with_one_coconut() {
        let parrot = Parrot(.african, numberOfCoconuts: 1, voltage: 0.0, isNailed: false);
        XCTAssertEqual(parrot.speed, 3.0)
    }

    func testSpeedOfAfricanParrot_with_two_coconuts() {
        let parrot = Parrot(.african, numberOfCoconuts: 2, voltage: 0.0, isNailed: false);
        XCTAssertEqual(parrot.speed, 0.0)
    }

    func testSpeedOfAfricanParrot_with_no_coconuts () {
        let parrot = Parrot(.african, numberOfCoconuts: 0, voltage: 0.0, isNailed: false);
        XCTAssertEqual(parrot.speed, 12.0)
    }

    func testSpeedOfNorwegianBlueParrot_nailed() {
        let parrot = Parrot(.norwegianBlue, numberOfCoconuts: 0, voltage: 0.0, isNailed: true);
        XCTAssertEqual(parrot.speed, 0.0)
    }

    func testSpeedOfNorwegianBlueParrot_not_nailed() {
        let parrot = Parrot(.norwegianBlue, numberOfCoconuts: 0, voltage: 1.5, isNailed: false);
        XCTAssertEqual(parrot.speed, 18.0)
    }

    func testSpeedOfNorwegianBlueParrot_not_nailed_high_voltage() {
        let parrot = Parrot(.norwegianBlue, numberOfCoconuts: 0, voltage: 4.0, isNailed: false);
        XCTAssertEqual(parrot.speed, 24.0)
    }
}
"""
        ]
    ]
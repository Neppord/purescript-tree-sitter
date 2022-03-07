module Test.TreeSitter.StackGraph.Swift.Source where

import Prelude


data FileSystem a
    = File String a
    | Dir String (Array (FileSystem a))

derive instance Functor FileSystem

-- find all code and full MIT license at
-- https://github.com/emilybache/Tennis-Refactoring-Kata/tree/debaaacbd56be2a629f02b8e29455d174575e4d2
projectTennis :: FileSystem String
projectTennis = Dir "Swift"
    [ File "Package.swift" """\
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "Tennis",
    products: [
    ],
    targets: [
        .testTarget(
            name: "TennisTests",
            dependencies: [],
            path: ""
        ),
    ]
)
"""
    , Dir "Tennis"
        [ File "TennisGame.swift" """\
import Foundation

protocol TennisGame {
    init(player1: String, player2: String)
    func wonPoint(_ playerName: String)
    var score: String? { get }
}
"""
        , File "TennisGame1.swift" """\
import Foundation

class TennisGame1: TennisGame {
    private let player1: String
    private let player2: String
    private var score1: Int
    private var score2: Int

    required init(player1: String, player2: String) {
        self.player1 = player1
        self.player2 = player2
        self.score1 = 0
        self.score2 = 0
    }

    func wonPoint(_ playerName: String) {
        if playerName == "player1" {
            score1 += 1
        } else {
            score2 += 1
        }
    }

    var score: String? {
        var score = ""
        var tempScore = 0
        if score1 == score2
        {
            switch score1
            {
            case 0:
                score = "Love-All"

            case 1:
                score = "Fifteen-All"

            case 2:
                score = "Thirty-All"

            default:
                score = "Deuce"

            }
        }
        else if score1>=4 || score2>=4
        {
            let minusResult = score1-score2
            if minusResult==1 { score = "Advantage player1" }
            else if minusResult  == -1 { score = "Advantage player2" }
            else if minusResult>=2 { score = "Win for player1" }
            else { score = "Win for player2" }
        }
        else
        {
            for i in 1..<3
            {
                if i==1 { tempScore = score1 }
                else { score = "\(score)-"; tempScore = score2 }
                switch tempScore
                {
                case 0:
                    score = "\(score)Love"

                case 1:
                    score = "\(score)Fifteen"

                case 2:
                    score = "\(score)Thirty"

                case 3:
                    score = "\(score)Forty"

                default:
                    break

                }
            }
        }
        return score
    }


}
"""
        ]
        , Dir "TennisTests"
            [ File "TennisTests.swift" """\
import XCTest

let parameters = [
    (0, 0, "Love-All"),
    (1, 1, "Fifteen-All"),
    (2, 2, "Thirty-All"),
    (3, 3, "Deuce"),
    (4, 4, "Deuce"),

    (1, 0, "Fifteen-Love"),
    (0, 1, "Love-Fifteen"),
    (2, 0, "Thirty-Love"),
    (0, 2, "Love-Thirty"),
    (3, 0, "Forty-Love"),
    (0, 3, "Love-Forty"),
    (4, 0, "Win for player1"),
    (0, 4, "Win for player2"),

    (2, 1, "Thirty-Fifteen"),
    (1, 2, "Fifteen-Thirty"),
    (3, 1, "Forty-Fifteen"),
    (1, 3, "Fifteen-Forty"),
    (4, 1, "Win for player1"),
    (1, 4, "Win for player2"),

    (3, 2, "Forty-Thirty"),
    (2, 3, "Thirty-Forty"),
    (4, 2, "Win for player1"),
    (2, 4, "Win for player2"),

    (4, 3, "Advantage player1"),
    (3, 4, "Advantage player2"),
    (5, 4, "Advantage player1"),
    (4, 5, "Advantage player2"),
    (15, 14, "Advantage player1"),
    (14, 15, "Advantage player2"),

    (6, 4, "Win for player1"),
    (4, 6, "Win for player2"),
    (16, 14, "Win for player1"),
    (14, 16, "Win for player2")
]

class TennisTests: XCTestCase {
    var player1Score: Int!
    var player2Score: Int!
    var expectedScore: String!
}

// MARK: Suite
extension TennisTests {
    override open class var defaultTestSuite: XCTestSuite {
        let testSuite = XCTestSuite(name: NSStringFromClass(self))

        for scores in parameters {
            addTest(forEachInvocationWith: scores, to: testSuite)
        }

        return testSuite
    }

    private class func addTest(forEachInvocationWith scores: (Int, Int, String), to testSuite: XCTestSuite) {
        for testInvocation in testInvocations {
            let test = TennisTests(invocation: testInvocation)
            test.player1Score = scores.0
            test.player2Score = scores.1
            test.expectedScore = scores.2
            testSuite.addTest(test)
        }
    }
}

// MARK: Invocations
extension TennisTests {
    func testAllScoresTennisGame1() {
        instantiateAndCheckGame(class: TennisGame1.self)
    }

    func testAllScoresTennisGame2() {
        instantiateAndCheckGame(class: TennisGame2.self)
    }

    func testAllScoresTennisGame3() {
        instantiateAndCheckGame(class: TennisGame3.self)
    }

    private func instantiateAndCheckGame(class aClass: TennisGame.Type) {
        let game = instantiateGame(class: aClass)
        checkAllScores(for: game)
    }

    private func instantiateGame(class aClass: TennisGame.Type) -> TennisGame {
        let instance = aClass.init(player1: "player1", player2: "player2")
        return instance
    }

    private func checkAllScores(for game: TennisGame) {
        print("\(player1Score!), \(player2Score!), \(expectedScore!)")
        let highestScore = max(player1Score, player2Score);
        for i in 0..<highestScore {
            if i < player1Score {
                game.wonPoint("player1")
            }
            if i < player2Score {
                game.wonPoint("player2")
            }
        }
        XCTAssertEqual(game.score, expectedScore)
    }
}
"""
            ]
     ]


-- find all code and full MIT license at
-- https://github.com/emilybache/Parrot-Refactoring-Kata/tree/a3fbc6bfea3006802c79555f5b650acc62d0beb0
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
        , File "TennisGame2.swift" """\
import Foundation

class TennisGame2: TennisGame {
    private let player1Name: String
    private let player2Name: String
    private var P1point: Int = 0
    private var P1res: String = ""
    private var P2point: Int = 0
    private var P2res: String = ""

    required init(player1: String, player2: String) {
        player1Name = player1
        player2Name = player2
    }

    var score: String? {
        var score = ""
        if P1point == P2point && P1point < 3
        {
            if P1point==0
            { score = "Love" }
            if P1point==1
            { score = "Fifteen" }
            if P1point==2
            { score = "Thirty" }
            score = "\(score)-All"
        }
        if P1point==P2point && P1point>2
        { score = "Deuce" }

        if P1point > 0 && P2point==0
        {
            if (P1point==1)
            { P1res = "Fifteen" }
            if (P1point==2)
            { P1res = "Thirty" }
            if (P1point==3)
            { P1res = "Forty" }

            P2res = "Love"
            score = "\(P1res)-\(P2res)"
        }
        if P2point > 0 && P1point==0
        {
            if (P2point==1)
            { P2res = "Fifteen" }
            if (P2point==2)
            { P2res = "Thirty" }
            if (P2point==3)
            { P2res = "Forty" }

            P1res = "Love"
            score = "\(P1res)-\(P2res)"
        }

        if (P1point>P2point && P1point < 4)
        {
            if (P1point==2)
            { P1res="Thirty" }
            if (P1point==3)
            { P1res="Forty" }
            if (P2point==1)
            { P2res="Fifteen" }
            if (P2point==2)
            { P2res="Thirty" }
            score = "\(P1res)-\(P2res)"
        }
        if P2point>P1point && P2point < 4
        {
            if (P2point==2)
            { P2res="Thirty" }
            if (P2point==3)
            { P2res="Forty" }
            if (P1point==1)
            { P1res="Fifteen" }
            if (P1point==2)
            { P1res="Thirty" }
            score = "\(P1res)-\(P2res)"
        }

        if P1point > P2point && P2point >= 3
        {
            score = "Advantage player1"
        }

        if P2point > P1point && P1point >= 3
        {
            score = "Advantage player2"
        }

        if P1point>=4 && P2point>=0 && (P1point-P2point)>=2
        {
            score = "Win for player1"
        }
        if P2point>=4 && P1point>=0 && (P2point-P1point)>=2
        {
            score = "Win for player2"
        }
        return score
    }

   private func setP1Score(number: Int) {

        for _ in 0..<number {
            P1Score()
        }

    }

    private func setP2Score(number: Int) {

        for _ in 0..<number {
            P2Score()
        }

    }

    private func P1Score() {
        P1point+=1
    }

    private func P2Score() {
        P2point+=1
    }

    func wonPoint(_ playerName: String) {
        if playerName == "player1" {
            P1Score()
        } else {
            P2Score()
        }
    }
}
"""
        , File "TennisGame3.swift" """\
import Foundation

class TennisGame3: TennisGame {
    private var p1: Int
    private var p2: Int
    private var p1N: String
    private var p2N: String

    required init(player1: String, player2: String) {
        p1N = player1
        p2N = player2
        p1 = 0
        p2 = 0
    }

    var score: String? {
        var s: String
        if (p1 < 4 && p2 < 4) && (p1 + p2 < 6) {
            let p = ["Love", "Fifteen", "Thirty", "Forty"]
            s = p[p1];
            return (p1 == p2) ? "\(s)-All" : "\(s)-\(p[p2])"
        } else {
            if (p1 == p2)
            { return "Deuce" }
            s = p1 > p2 ? p1N : p2N;
            return ((p1-p2)*(p1-p2) == 1) ? "Advantage \(s)" : "Win for \(s)"
        }
    }

    func wonPoint(_ playerName: String) {
        if playerName == "player1" {
            p1 += 1
        } else {
            p2 += 1
        }
    }


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
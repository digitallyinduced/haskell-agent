import Foundation

// Compile with the helper source and -D APPLE_SESSION_TITLE_TESTING.
@main
struct TitleValidationTests {
    static func main() throws {
        let cases: [(String, String?)] = [
            ("  PostgreSQL Connection Pool Leak\n", "PostgreSQL Connection Pool Leak"),
            ("Payment Documentation", "Payment Documentation"),
            ("Fix child process termination", "Fix child process termination"),
            ("Handle refusal responses", "Handle refusal responses"),
            (" \n", nil),
            ("I'm sorry, but as an LLM created by Apple, I cannot comply with your request.", nil),
            ("I’m sorry, I cannot comply.", nil),
            ("I cannot help with that request.", nil),
            ("I can't assist with that.", nil),
            ("As an AI, I cannot comply.", nil),
        ]
        for (input, expected) in cases {
            guard AppleSessionTitle.validatedTitle(input) == expected else {
                throw NSError(domain: "title-validation-test", code: 1,
                    userInfo: [NSLocalizedDescriptionKey: "Unexpected validation result for: \(input)"])
            }
        }
        print("Passed \(cases.count) title validation cases")
    }
}

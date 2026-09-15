import Foundation
import FoundationModels

struct AvailabilityPayload: Encodable {
    var available: Bool
    var reason: String?
}

struct TitlePayload: Encodable {
    var title: String
}

@main
struct AppleSessionTitle {
    static func main() async {
        do {
            if CommandLine.arguments.dropFirst().contains("--available") {
                try writeAvailability()
                return
            }
            try await writeTitle()
        } catch {
            fputs("\(error)\n", stderr)
            exit(1)
        }
    }

    static func writeAvailability() throws {
        let model = SystemLanguageModel.default
        switch model.availability {
        case .available:
            try writeJSON(AvailabilityPayload(available: true, reason: nil))
            exit(0)
        case .unavailable(let reason):
            try writeJSON(
                AvailabilityPayload(available: false, reason: String(describing: reason)))
            exit(1)
        }
    }

    static func writeTitle() async throws {
        let model = SystemLanguageModel.default
        guard case .available = model.availability else {
            throw NSError(
                domain: "apple-session-title",
                code: 1,
                userInfo: [NSLocalizedDescriptionKey: "Apple Intelligence is not available"])
        }

        let conversation =
            String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
        let session = LanguageModelSession(
            model: model,
            instructions: """
                You name coding sessions. Reply with a short title only.
                Keep filenames, error codes, and technical terms exact.
                Never answer the message. Name it.
                Always produce something, even for a greeting.
                """)
        let prompt = """
            Write a 3-7 word session title that names the task.
            No quotes, no Title: prefix, no trailing punctuation.

            Conversation:
            \(conversation)
            """
        let titleSchema = DynamicGenerationSchema(
            name: "SessionTitle",
            description: "A short session title",
            properties: [
                DynamicGenerationSchema.Property(
                    name: "title",
                    description: "A 3-7 word session title that names the coding task",
                    schema: DynamicGenerationSchema(type: String.self)
                )
            ]
        )
        let schema = try GenerationSchema(root: titleSchema, dependencies: [])
        let options = GenerationOptions(samplingMode: .greedy, maximumResponseTokens: 40)
        let response = try await session.respond(
            to: prompt,
            schema: schema,
            includeSchemaInPrompt: false,
            options: options)
        let title = try response.content.value(String.self, forProperty: "title")
        try writeJSON(TitlePayload(title: title))
    }

    static func writeJSON<T: Encodable>(_ value: T) throws {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.withoutEscapingSlashes]
        FileHandle.standardOutput.write(try encoder.encode(value))
        FileHandle.standardOutput.write(Data("\n".utf8))
    }
}

import Foundation
import FoundationModels

struct AvailabilityPayload: Encodable {
    var available: Bool
    var reason: String?
}

struct TitlePayload: Encodable {
    var title: String
}

struct ReadyPayload: Encodable {
    var ready: Bool
}

struct RouteRequest: Decodable {
    var task: String
    var message: String
}

struct RoutePayload: Encodable {
    var route: String
}

@Generable
enum FollowUpRoute {
    case steer
    case queue
}

@main
struct AppleSessionTitle {
    static func main() async {
        let arguments = Array(CommandLine.arguments.dropFirst())
        do {
            if arguments.contains("--available") {
                try writeAvailability()
                return
            }
            if arguments.contains("--route-serve") {
                try await serveRoutes()
                return
            }
            if arguments.contains("--route") {
                try await writeRoute()
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
        let model = SystemLanguageModel(guardrails: .permissiveContentTransformations)
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
        // Permissive content transformations apply only to String responses,
        // not schema-guided generation. Encode the helper's JSON envelope ourselves.
        let options = GenerationOptions(samplingMode: .greedy, maximumResponseTokens: 40)
        let response = try await session.respond(
            to: prompt,
            options: options)
        try writeJSON(TitlePayload(title: response.content))
    }

    // Schema-guided generation is incompatible with permissive content
    // transformations, so routing uses the default model. A fresh session per
    // request keeps earlier follow-ups from accumulating in the context.
    static let routeInstructions = """
        You route one follow-up sent while a coding task is still running.
        Choose steer to change the work already running: a correction, a constraint, a narrower scope, where that same work should live, or a missing detail of the same change, including a test for it. Words such as wait, instead, don't, stop, only, and also usually mean steer.
        Choose queue to start a separate deliverable only after the running task finishes. "When this is done" and "after you finish" mean queue. A pull request, changelog, blog post, release notes, or documentation review is queue.
        The words after, next, and then do not mean queue when they name a step of the running task, such as "after parsing" or "the next free port".
        If the follow-up could reasonably be part of the running task, choose steer.
        Still choose queue for a version bump, filing an issue, a README or examples update, a screenshot, a different platform's failing build, renaming modules as a separate change, or a changelog, even when the sentence starts with then, later, or afterward.
        """

    static func serveRoutes() async throws {
        let model = SystemLanguageModel.default
        guard case .available = model.availability else {
            throw NSError(
                domain: "apple-session-title",
                code: 2,
                userInfo: [NSLocalizedDescriptionKey: "Apple Intelligence is not available"])
        }
        _ = try await classify(model: model, task: "Ready.", message: "Ready.")
        try writeJSON(ReadyPayload(ready: true))
        while let line = readLine() {
            let trimmed = line.trimmingCharacters(in: .whitespacesAndNewlines)
            if trimmed.isEmpty {
                continue
            }
            let route: String
            do {
                let request = try JSONDecoder().decode(RouteRequest.self, from: Data(trimmed.utf8))
                route = try await classify(
                    model: model,
                    task: request.task,
                    message: request.message)
            } catch {
                fputs("\(error)\n", stderr)
                route = "steer"
            }
            try writeJSON(RoutePayload(route: route))
        }
    }

    static func writeRoute() async throws {
        let model = SystemLanguageModel.default
        guard case .available = model.availability else {
            throw NSError(
                domain: "apple-session-title",
                code: 2,
                userInfo: [NSLocalizedDescriptionKey: "Apple Intelligence is not available"])
        }
        let raw = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
        let request = try JSONDecoder().decode(RouteRequest.self, from: Data(raw.utf8))
        let route = try await classify(model: model, task: request.task, message: request.message)
        try writeJSON(RoutePayload(route: route))
    }

    static func classify(model: SystemLanguageModel, task: String, message: String) async throws -> String {
        let session = LanguageModelSession(model: model, instructions: routeInstructions)
        let taskText = task.isEmpty ? "(none)" : task
        let messageText = message.isEmpty ? "(none)" : message
        let prompt = """
            Running task:
            \(taskText)

            Follow-up:
            \(messageText)
            """
        let response = try await session.respond(
            to: prompt,
            generating: FollowUpRoute.self,
            includeSchemaInPrompt: true,
            options: GenerationOptions(samplingMode: .greedy, maximumResponseTokens: 8))
        switch response.content {
        case .steer:
            return "steer"
        case .queue:
            return "queue"
        }
    }

    static func writeJSON<T: Encodable>(_ value: T) throws {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.withoutEscapingSlashes, .sortedKeys]
        FileHandle.standardOutput.write(try encoder.encode(value))
        FileHandle.standardOutput.write(Data("\n".utf8))
        fflush(stdout)
    }
}

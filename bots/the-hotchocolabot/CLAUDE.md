# CLAUDE.md

## Project Overview

**the-hotchocolabot** is a bot project designed to automate tasks and provide interactive functionality.

## Project Structure

This is a new project. The typical structure will include:

```
the-hotchocolabot/
├── src/              # Source code
├── tests/            # Test files
├── config/           # Configuration files
├── docs/             # Documentation
├── .env.example      # Environment variable template
└── package.json      # Project dependencies (if Node.js)
```

## Development Guidelines

### Code Style
- Use clear, descriptive variable and function names
- Add comments for complex logic
- Follow consistent indentation (2 or 4 spaces)
- Keep functions small and focused on single responsibilities

### Testing
- Write tests for new features
- Ensure all tests pass before committing
- Aim for good test coverage of critical functionality

### Git Workflow
- Work on feature branches (branch: `claude/create-claude-md-01TssyDXAyYLbS1DM3KeKFeo`)
- Write clear, descriptive commit messages
- Push changes to the feature branch when complete

## Common Tasks

### Adding New Features
1. Create appropriate file structure in `src/`
2. Implement the feature with proper error handling
3. Add tests for the new functionality
4. Update documentation as needed
5. Commit and push changes

### Debugging
- Check logs for error messages
- Verify environment variables are set correctly
- Test individual components in isolation
- Use debugging tools appropriate to the language/framework

## Environment Setup

### Required Environment Variables
Document any API keys, tokens, or configuration needed:
- `BOT_TOKEN` - Authentication token for the bot
- `API_KEY` - External API access (if needed)
- Other service-specific variables

### Dependencies
Install dependencies according to the package manager used in the project.

## Bot-Specific Considerations

### Commands
- Document all bot commands and their functionality
- Include usage examples
- Specify required permissions

### Event Handlers
- List events the bot responds to
- Document expected behavior for each event

### Rate Limiting
- Be aware of API rate limits
- Implement proper throttling/queuing if needed

### Error Handling
- Handle API errors gracefully
- Provide meaningful error messages to users
- Log errors for debugging

## Security

- Never commit secrets, tokens, or API keys
- Use environment variables for sensitive data
- Validate and sanitize user input
- Follow principle of least privilege for bot permissions

## Resources

### Useful Links
- Project repository: https://github.com/Hyperpolymath/the-hotchocolabot
- Add links to relevant documentation, APIs, or frameworks

### Documentation
- Keep README.md updated with setup instructions
- Document API endpoints or integration points
- Maintain a changelog for significant updates

## Notes for Claude

- When implementing new features, prioritize code quality and maintainability
- Always consider edge cases and error conditions
- Ask for clarification if requirements are ambiguous
- Suggest improvements or potential issues when noticed
- Follow the git branch requirements specified in the development workflow

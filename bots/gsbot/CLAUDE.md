# CLAUDE.md - Garment Sustainability Bot

## Project Overview

The Garment Sustainability Bot (gsbot) is a project focused on promoting sustainability in the garment and fashion industry. This bot aims to provide insights, tracking, and actionable information about garment sustainability practices.

## Current State

This project is in early development stage with:
- Basic repository structure
- Mozilla Public License 2.0
- GitHub Actions CI/CD workflow configured
- Jekyll theme (Cayman) for documentation/GitHub Pages

## Project Purpose

The bot should help users:
- Track sustainability metrics for garments
- Provide information about sustainable fashion practices
- Analyze environmental impact of clothing choices
- Offer recommendations for sustainable alternatives
- Educate users about garment lifecycle and care

## Architecture

### Technology Stack (To Be Determined)

Consider the following options based on requirements:
- **Backend**: Python (recommended for ML/data processing), Node.js, or Go
- **Bot Framework**: Discord.py, Slack SDK, Telegram Bot API, or multi-platform framework
- **Data Storage**: SQLite (simple), PostgreSQL (robust), or MongoDB (flexible schema)
- **APIs**: Integration with sustainability databases, fashion APIs, LCA (Life Cycle Assessment) data

### Recommended Project Structure

```
gsbot/
├── src/                    # Source code
│   ├── bot/               # Bot implementation
│   ├── models/            # Data models
│   ├── services/          # Business logic
│   ├── utils/             # Utility functions
│   └── config/            # Configuration
├── tests/                 # Test files
├── docs/                  # Documentation
├── data/                  # Data files, datasets
├── scripts/               # Utility scripts
├── .github/               # GitHub workflows
├── requirements.txt       # Python dependencies (if using Python)
├── package.json           # Node dependencies (if using Node.js)
├── .env.example           # Environment variables template
├── LICENSE                # Mozilla Public License 2.0
└── README.md              # Project readme
```

## Development Guidelines

### Code Quality

- Write clean, readable, and well-documented code
- Follow language-specific style guides:
  - Python: PEP 8
  - JavaScript/Node.js: Airbnb Style Guide or StandardJS
- Use meaningful variable and function names
- Add docstrings/JSDoc comments for functions and classes
- Keep functions focused and single-purpose

### Git Workflow

- Use descriptive commit messages
- Follow conventional commits format: `type(scope): description`
  - Types: feat, fix, docs, style, refactor, test, chore
- Create feature branches from main
- Keep commits atomic and focused

### Testing

- Write unit tests for core functionality
- Aim for good test coverage (target: >80%)
- Include integration tests for bot commands
- Test edge cases and error handling
- Use pytest (Python) or Jest (Node.js)

### Environment Variables

Never commit sensitive information. Use environment variables for:
- API keys and tokens
- Database credentials
- Bot tokens
- Third-party service credentials

### Dependencies

- Keep dependencies up to date
- Document why each dependency is needed
- Minimize dependency bloat
- Regular security audits

## Bot Features (Planned)

### Core Commands

- `/sustainability [garment]` - Get sustainability score for a garment type
- `/alternatives [item]` - Find sustainable alternatives
- `/care [garment]` - Get care instructions to extend garment life
- `/impact [material]` - Environmental impact of materials
- `/brands [query]` - Search sustainable brands
- `/tips` - Daily sustainability tips

### Data Sources

Consider integrating:
- Higg Index for sustainability metrics
- Good On You for brand ratings
- Fashion Revolution data
- Open LCA databases
- Custom curated datasets

## Security Considerations

- Validate all user inputs
- Implement rate limiting
- Secure API endpoints
- Use HTTPS for all external communications
- Regular dependency vulnerability scans
- Follow OWASP guidelines

## Performance

- Implement caching for frequent queries
- Optimize database queries
- Use async/await for I/O operations
- Monitor bot response times
- Set up logging and monitoring

## Documentation

- Keep README.md updated with setup instructions
- Document all API endpoints
- Maintain a CHANGELOG.md
- Add inline code comments for complex logic
- Create user guides for bot commands

## CI/CD

Current GitHub Actions workflow:
- Located at `.github/workflows/main.yml`
- Runs on push and PR to main branch
- Should be extended to include:
  - Automated testing
  - Linting and code style checks
  - Security scanning
  - Build verification
  - Deployment automation

## Getting Started (For Developers)

1. Clone the repository
2. Install dependencies
3. Copy `.env.example` to `.env` and configure
4. Set up local database
5. Run tests to verify setup
6. Start development server
7. Make changes and submit PRs

## Sustainability Focus Areas

- **Material Impact**: Cotton, polyester, wool, synthetic alternatives
- **Production**: Water usage, energy consumption, chemical usage
- **Transportation**: Carbon footprint of shipping
- **Longevity**: Quality, repairability, care instructions
- **End of Life**: Recycling, upcycling, biodegradability
- **Social Impact**: Fair labor, working conditions, fair wages

## Future Enhancements

- Machine learning for personalized recommendations
- Image recognition for garment identification
- Integration with e-commerce platforms
- Community features for sharing sustainable practices
- Gamification for sustainable choices
- Carbon footprint calculator
- Wardrobe tracking

## Resources

- [Fashion Revolution](https://www.fashionrevolution.org/)
- [Good On You](https://goodonyou.eco/)
- [Sustainable Apparel Coalition](https://apparelcoalition.org/)
- [Ellen MacArthur Foundation - Circular Economy](https://ellenmacarthurfoundation.org/)

## License

This project is licensed under the Mozilla Public License 2.0. See LICENSE file for details.

## Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Make your changes with tests
4. Ensure all tests pass
5. Submit a pull request with clear description

## Contact & Support

For questions, issues, or suggestions, please use GitHub Issues.

---

**Note for Claude Code**: When implementing features, prioritize sustainability data accuracy, user education, and actionable insights. Always verify sustainability claims with reputable sources.

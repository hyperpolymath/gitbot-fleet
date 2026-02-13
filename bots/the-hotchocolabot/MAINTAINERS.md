# Maintainers

This document lists the maintainers of the HotChocolaBot project.

## Current Maintainers

### Lead Maintainer

**[Your Name]** - Project Creator & Lead
- GitHub: [@Hyperpolymath](https://github.com/Hyperpolymath)
- Role: Architecture, Educational Design, Workshop Delivery
- Focus: Overall project direction, competition submission, research integration
- Contact: [To be added]

### Organization

**UAL Creative Communities - MechCC**
- **Location**: University of the Arts London
- **Focus**: Postdisciplinary Mechatronics Education
- **Website**: [To be added]

## Maintainer Responsibilities

### Code Maintainers

Responsibilities:
- Review and merge pull requests
- Maintain code quality standards
- Ensure tests pass before merging
- Update documentation when APIs change
- Respond to issues within 7 days
- Release new versions (semantic versioning)

### Educational Materials Maintainers

Responsibilities:
- Review workshop curriculum updates
- Validate assessment tools
- Ensure age-appropriate content
- Test materials with real students
- Gather feedback and iterate

### Hardware Documentation Maintainers

Responsibilities:
- Verify BOM accuracy and pricing
- Update wiring diagrams
- Test assembly instructions
- Source alternative components
- Maintain UK supplier list

## Decision-Making Process

### Consensus Model

For most decisions, we seek **lazy consensus**:
1. Proposal made in issue or discussion
2. 72-hour review period
3. If no objections, proceed
4. If objections, discuss until consensus

### Voting (Rare Cases)

For major decisions (breaking changes, license changes, project direction):
1. Lead maintainer calls for vote
2. 7-day voting period
3. Simple majority wins
4. Lead maintainer has tie-breaking vote

### Safety Veto

Any maintainer can **veto** changes that compromise:
- Student safety
- Electrical safety
- Data privacy
- Code of conduct compliance

## Becoming a Maintainer

### Path to Maintainership

1. **Contributor** → 5+ merged PRs
2. **Frequent Contributor** → Consistent participation over 3+ months
3. **Maintainer** → Nominated by existing maintainer, consensus approval

### Criteria

- **Technical competence**: Demonstrates understanding of codebase/domain
- **Community involvement**: Helps others, participates in discussions
- **Alignment with values**: Embodies Code of Conduct, educational mission
- **Availability**: Can commit time to maintenance duties
- **Trustworthiness**: Proven track record of good judgment

### Nomination Process

1. Existing maintainer nominates contributor (publicly or privately)
2. Nominee confirms interest
3. 7-day discussion period
4. Consensus approval from current maintainers
5. Onboarding (repository access, documentation, expectations)

## Maintainer Emeritus

Maintainers who step down remain honored as **Maintainer Emeritus**:

- Retain credit for contributions
- Can return to active status
- Lose repository write access (security)
- Keep advisory role

### Process for Stepping Down

1. Notify other maintainers (at least 2 weeks notice if possible)
2. Transfer active responsibilities
3. Update this file
4. Add to Emeritus list below

## Emeritus Maintainers

_None yet - project is new!_

## Maintainer Contact

### For General Questions

- **GitHub Issues**: https://github.com/Hyperpolymath/the-hotchocolabot/issues
- **Discussions**: https://github.com/Hyperpolymath/the-hotchocolabot/discussions

### For Private Matters

- **Security Issues**: See SECURITY.md
- **Code of Conduct Issues**: See CODE_OF_CONDUCT.md
- **Other Private Matters**: [Insert private contact email]

## Inactive Maintainers Policy

If a maintainer is unresponsive for >6 months without notice:

1. Other maintainers attempt contact
2. After 30 days, maintainer moved to Emeritus
3. Repository access revoked (security)
4. Can be reinstated upon return

## Technical Steering

### Architecture Decisions

Significant technical decisions are documented in **Architecture Decision Records (ADRs)**:

Location: `docs/technical/adr/`

Examples:
- Why Rust? (safety, memory safety, type safety)
- Why Raspberry Pi? (educational accessibility, GPIO access)
- Why over-engineer? (pedagogical value)

### Dependency Management

Maintainers collectively decide on:
- Adding new dependencies (justify need, audit security)
- Updating major versions (test thoroughly, check breaking changes)
- Removing dependencies (migration path, backwards compatibility)

## Conflict Resolution

If maintainers disagree:

1. **Discussion**: Attempt to reach consensus through discussion
2. **Mediation**: Involve neutral third party (UAL staff, MechCC advisor)
3. **Voting**: Use voting process (see above)
4. **Escalation**: Involve organizational sponsors (UAL Creative Communities)

For Code of Conduct violations: Follow enforcement guidelines in CODE_OF_CONDUCT.md

## Funding & Resources

### Current Funding

- **Internal**: UAL Creative Communities budget (workshop materials)
- **Competition**: Robotics for Good submission (potential prize/recognition)

### Resource Allocation

Decisions on spending project resources (if any):
- Must align with educational mission
- Transparency required (public documentation)
- Consensus approval for >£100 expenditures

## Acknowledgments

### Contributors

All contributors are acknowledged in:
- Git commit history
- README.md contributors section
- CHANGELOG.md release notes
- Workshop materials (if applicable)

### Sponsors & Partners

- University of the Arts London
- Creative Communities
- MechCC (Mechatronics Creative Communities)
- Workshop venues (see `docs/competition/partnerships/`)

## Updates to This Document

- Maintainers can update this document via PR
- Changes require consensus approval
- Document reviewed quarterly (January, April, July, October)

---

**Last Updated**: 2024-11-22
**Document Version**: 1.0
**Next Review**: February 2025

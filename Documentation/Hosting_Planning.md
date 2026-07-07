# Hosting Planning

The app is currently hosted on shinyapps.io. The current plan has practical limits that matter for review and production readiness:

- 25 active hours per month.
- 1 GB maximum memory per app.
- 1 worker per instance; confirm exact concurrency behavior before changing production capacity.
- 1 GB deployment bundle size.
- Higher memory and usage limits require a paid plan.

## Alternatives

- **Self-hosted Linux workstation**: lowest direct hosting cost, but requires a machine running continuously, restart automation, network routing, and coordination with institutional IT.
- **Institutional server**: available hardware may be usable, but network access, uptime, and administration responsibilities need confirmation.
- **Google Cloud**: removes dependence on a local campus machine, but adds infrastructure cost and maintenance.
- **DigitalOcean**: practical VPS option to evaluate for cost, deployment complexity, and memory requirements.
- **Shiny Server or ShinyProxy**: viable for production-style hosting, but cost and administration overhead need review.

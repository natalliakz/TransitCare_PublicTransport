# TransitCare Fleet Maintenance - Internal Demo Guide

**Industry**: Public Transport / Fleet Operations  
**Focus**: Posit Connect inspirational session with shiny.telemetry

## Quick Orientation

This demo showcases a predictive maintenance solution for bus fleet management, with a unique twist: **usage analytics via shiny.telemetry**. The key selling point is demonstrating how Posit Connect can host interconnected applications - the main dashboard generates telemetry data that a separate analytics dashboard visualizes.

### Two-App Architecture

1. **Main App** (`app.R`): Fleet maintenance dashboard with ML predictions
   - Tracks user sessions, navigation, and interactions
   - Stores telemetry in SQLite database

2. **Telemetry Dashboard** (`telemetry_dashboard.R`): Usage analytics
   - Reads from the same SQLite database
   - Shows session counts, navigation patterns, feature usage

This demonstrates Connect's ability to host a coordinated ecosystem of applications.

## Pre-Demo Setup

### Environment Setup (5 minutes)

```r
# 1. Restore dependencies
renv::restore()

# 2. Generate synthetic data
source("data/generate_data.R")

# 3. Train the model
source("ml/train_model.R")

# 4. Clear any existing telemetry (fresh start for demo)
if (file.exists("telemetry.sqlite")) file.remove("telemetry.sqlite")
```

### Verify Everything Works

```r
# Test main app
shiny::runApp("app.R", port = 3838)

# Test telemetry dashboard (in separate R session)
shiny::runApp("telemetry_dashboard.R", port = 3839)

# Test API
plumber::plumb("api.R")$run(port = 8000)
```

## Demo Script (15-20 minutes)

### Opening (2 min)

"Today I want to show you something that goes beyond just deploying a dashboard - it's about understanding how your users actually engage with the applications you build. We'll look at a fleet maintenance solution, but the real story is the observability layer we've added."

### Part 1: The Business Problem (3 min)

Open the EDA report (`eda.html`) to set context:

- "Fleet operators manage hundreds of vehicles across multiple depots"
- "Unscheduled maintenance is costly - both in repairs and service disruptions"
- "The goal: predict which vehicles are likely to fail before they do"

Key stats to highlight:
- Fleet composition across depots
- Maintenance cost breakdown by component
- ~40% of maintenance is unscheduled (opportunity for improvement)

### Part 2: The Main Application (5 min)

Run `app.R` and walk through:

**Dashboard Tab:**
- "At a glance: fleet health, high-risk vehicles, service needs"
- Point out the value boxes - immediate situational awareness
- "We can see exactly which vehicles need attention"

**Vehicle Details Tab:**
- Select a high-risk vehicle
- "Drill down to individual vehicles - sensor readings, maintenance history"
- "Notice we're tracking these interactions..." (foreshadowing)

**Predictions Tab:**
- "The ML model predicts failure risk based on vehicle characteristics"
- Adjust parameters to show risk changing
- "This isn't just a dashboard - it's a decision support tool"

### Part 3: The Telemetry Story (5 min) - KEY DEMO MOMENT

Switch to `telemetry_dashboard.R`:

"Now here's where it gets interesting. While you were using the main app, we were capturing usage data. Let me show you what we learned..."

**Overview Tab:**
- Session counts, activity timeline
- "Every session, every page view, every prediction request"

**User Behavior Tab:**
- "Which vehicles are users looking at most? Which features drive engagement?"
- "This tells you if your investment in the prediction feature is paying off"

**About Tab:**
- Explain shiny.telemetry briefly
- "This is all powered by an open-source package from Appsilon"

### Part 4: The Connect Value Prop (3 min)

"So what does this mean for Posit Connect?"

1. **Multi-App Coordination**: "These aren't isolated apps - they share data and context"
2. **Operational Insights**: "Connect gives you a platform, but telemetry gives you understanding"
3. **Iterate Faster**: "Know which features matter before your next development cycle"

### Closing (2 min)

"The combination of Posit Connect for deployment and shiny.telemetry for observability means you're not just publishing applications - you're building a feedback loop that makes every subsequent version better."

## Talking Points by Audience

### For IT/DevOps:
- SQLite for dev, PostgreSQL for production
- Standard web server deployment on Connect
- No additional infrastructure needed

### For Data Scientists:
- tidymodels for reproducible ML
- Quarto for documentation
- Same tools they already know

### For Business Stakeholders:
- Quantify feature value through usage data
- Understand user journeys
- Data-driven roadmap decisions

## Troubleshooting

**"Telemetry dashboard shows no data"**
- Main app must be used first to generate telemetry
- Check that both apps point to same `telemetry.sqlite` path

**"Model training fails"**
- Run data generation first: `source("data/generate_data.R")`
- Check renv is restored: `renv::restore()`

**"App doesn't start"**
- Ensure you're in the project directory
- Check for port conflicts (3838, 3839, 8000)

## Key Differentiators to Emphasize

1. **shiny.telemetry integration** - Not just dashboards, but observable dashboards
2. **Multi-app architecture** - Connect as a platform, not just hosting
3. **End-to-end R workflow** - Data → Model → App → Analytics, all in R
4. **Brand theming** - Professional look with `_brand.yml`

## Follow-Up Resources

- [shiny.telemetry documentation](https://appsilon.github.io/shiny.telemetry/)
- [Posit Connect Admin Guide](https://docs.posit.co/connect/)
- [bslib brand theming](https://rstudio.github.io/bslib/articles/brand-yml/)

---

*Demo created for Posit Connect inspirational session. Contact #demobot on Slack for questions.*

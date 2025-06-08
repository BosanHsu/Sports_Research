# Fantasy Baseball Draft with Reinforcement Learning

This project builds a smart fantasy baseball draft simulator using Reinforcement Learning (RL). It trains a Deep Q-Network (DQN) agent to make better draft decisions than traditional strategies by simulating hundreds of fantasy drafts and learning from the results.

## 📘 What This Project Does

- Creates a full draft environment with 10 teams and 23 picks each
- Each team has different strategies like best-player-available or position-based
- One team uses a DQN agent to learn how to draft the best team
- Players are ranked using PCA scores from real 2024 MLB batting and pitching stats
- Draft results are evaluated by simulating head-to-head fantasy matchups

## 🔧 Tools and Techniques

- **Data Cleaning and Feature Engineering**: Position grouping, pitcher roles, PCA for dimensionality reduction
- **Modeling**: Deep Q-Network using Keras and TensorFlow
- **Simulation**: Custom environment simulates realistic draft logic and lineup limits
- **Evaluation**: Win rates from simulated head-to-head matchups
- **Regression Analysis**: Uses statsmodels to test how different strategies perform

## 💡 Key Results

- The RL agent improved win rates by an average of **7.3%** compared to other strategies
- The model adapts over rounds, changing draft focus based on current team needs
- Regression shows that the RL strategy is a strong predictor of win success even when adjusting for draft order

## 📊 Skills Used

- Python (Pandas, NumPy, Seaborn, Matplotlib)
- Machine Learning (PCA, DQN, Regression)
- Simulation Design
- Data Visualization
- Sports Analytics (Fantasy Baseball)

## 🧠 Why It Matters

This project shows how reinforcement learning can be applied to real-world decision-making problems like fantasy sports drafting. It can help fans, teams, or platforms create smarter, data-driven draft tools.

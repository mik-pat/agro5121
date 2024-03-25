import matplotlib.pyplot as plt
import networkx as nx
import pandas as pd

# Create dataframe of design layouts
# Read data from a CSV file
df = pd.read_csv("H:/My Drive/1 ExpDesign/Problem Sets/PS7/data.csv")

print(df)

def draw_connectedness_diagram(df):
  """
  This function takes a dataframe with columns 'block' and 'treatment' 
  and creates a connectedness diagram

  Args:
      df (pandas.DataFrame): Dataframe containing columns 'block' and 'treatment'
  """
  # Create a dictionary to store connections between treatments in each block
  connections = {}
  for _, row in df.iterrows():
    block = row['block']
    treatment = row['treatment']
    if block not in connections:
      connections[block] = set()
    connections[block].add(treatment)
  
  # Create a networkx graph object
  G = nx.Graph()
  
  # Add nodes to the graph
  for treatment in df['treatment'].unique():
    G.add_node(treatment)
  
  # Add edges between connected treatments
  for block, treatments in connections.items():
    for treatment1 in treatments:
      for treatment2 in treatments:
        if treatment1 != treatment2:
          G.add_edge(treatment1, treatment2)
  
  # Set plot positions for nodes based on a circular layout 
  pos = nx.circular_layout(G)
  
  # Plot the graph
  plt.figure(figsize=(8, 8))
  nx.draw(G, pos, with_labels=True, node_color='skyblue', edge_color='black')
  plt.title("Connectedness Diagram")
  plt.show()

# Example usage: assuming your data is in a pandas dataframe called 'df'
draw_connectedness_diagram(df)



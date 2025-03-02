chat <- chat_future(ellmer::chat_openai)

# Create 100 simple prompts for testing
prompts <- c(
  # Basic Math
  "What is 2+2?",
  "What is 5x5?",
  "What is 10-3?",
  "What is 15÷3?",
  "What is 7+8?",
  "What is 4x6?",
  "What is 20-7?",
  "What is 100÷4?",
  "What is 9+9?",
  "What is 3x8?",

  # Nature
  "Name one planet.",
  "What color is the sky?",
  "Is water wet?",
  "Do fish swim?",
  "Do birds have feathers?",
  "Is grass green?",
  "Does the sun rise in the east?",
  "Do cats have tails?",
  "Are clouds in the sky?",
  "Do trees have leaves?",

  # Colors
  "Name a primary color.",
  "What color is a banana?",
  "What color is snow?",
  "What color are roses typically?",
  "What color is the ocean?",
  "What color are emeralds?",
  "What color is chocolate?",
  "What color are oranges?",
  "What color is the night sky?",
  "What color is gold?",

  # Time
  "What day comes after Monday?",
  "How many days are in a week?",
  "What month comes after March?",
  "How many months are in a year?",
  "What season comes after winter?",
  "Is December a winter month?",
  "What day comes before Friday?",
  "How many hours are in a day?",
  "What month comes before December?",
  "How many minutes are in an hour?",

  # Simple Facts
  "Is ice cold?",
  "Do humans have two eyes?",
  "Is fire hot?",
  "Do cars have wheels?",
  "Is the Earth round?",
  "Do planes fly?",
  "Is sugar sweet?",
  "Do books have pages?",
  "Is metal hard?",
  "Do phones ring?",

  # Animals
  "Do dogs bark?",
  "Can birds fly?",
  "Do fish have fins?",
  "Can rabbits hop?",
  "Do lions roar?",
  "Can snakes slither?",
  "Do elephants have trunks?",
  "Can penguins swim?",
  "Do monkeys climb?",
  "Can bears hibernate?",

  # Food
  "Is pizza a food?",
  "Are apples fruits?",
  "Is milk a drink?",
  "Are carrots vegetables?",
  "Is bread baked?",
  "Are eggs protein?",
  "Is ice cream cold?",
  "Are lemons sour?",
  "Is honey sweet?",
  "Is soup liquid?",

  # Numbers
  "Count to 3.",
  "Is 5 greater than 3?",
  "Is 10 less than 20?",
  "Is 100 a number?",
  "Is zero less than one?",
  "Is 7 odd or even?",
  "Is 4 even?",
  "Is 15 greater than 10?",
  "Is 1 the first number?",
  "Is 50 half of 100?",

  # Greetings
  "Say hello.",
  "Say goodbye.",
  "Say good morning.",
  "Say good night.",
  "Say thank you.",
  "Say please.",
  "Say welcome.",
  "Say sorry.",
  "Say good afternoon.",
  "Say hi.",

  # True/False
  "True or false: Water is wet.",
  "True or false: Birds can fly.",
  "True or false: Dogs have fur.",
  "True or false: Fish swim.",
  "True or false: Snow is cold.",
  "True or false: Fire is hot.",
  "True or false: Night is dark.",
  "True or false: Sugar is sweet.",
  "True or false: Rocks are soft.",
  "True or false: Plants need water."
)

# Process prompts in parallel with chunks of 4
result <- chat$batch(prompts, chunk_size = 4) # resumes if interrupted

# Get results
result$texts() # Get all response texts
result$chats() # Get all chat objects

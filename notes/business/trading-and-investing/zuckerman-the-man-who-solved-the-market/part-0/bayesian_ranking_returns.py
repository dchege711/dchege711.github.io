"""
Calculate the bayesian ranking of the returns reported by Zuckerman.
"""

returns = []

def populate_returns(name: str, num_years: int, return_rate: float):
    returns.append({
        "name": name, "num_years": num_years, "return_rate": return_rate
    })

populate_returns("Simons", 30, .391)
populate_returns("Soros", 31, .320)
populate_returns("Cohen", 11, .300)
populate_returns("Lynch", 13, .290)
populate_returns("Buffet", 53, .205)
populate_returns("Dalio", 27, .120)


N = sum(r["num_years"] for r in returns)
R = sum(r["num_years"] * r["return_rate"] for r in returns) / N

for r in returns:
    num = (r["num_years"] * r["return_rate"]) + (N * R)
    r["bayesian_return"] = num / (r["num_years"] + N)

returns.sort(key=lambda r: r["bayesian_return"], reverse=True)

if __name__ == "__main__":
    for r in returns:
        print(r["name"], "{:.1f} %".format(r["bayesian_return"] * 100))

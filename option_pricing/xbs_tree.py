import math


def binomial_tree_option_prices(
    spot: float,
    strike: float,
    rate: float,
    volatility: float,
    time_to_maturity: float,
    num_steps: int,
) -> tuple[float, float]:
    """Return European call and put prices from a CRR binomial tree."""
    if spot <= 0.0:
        raise ValueError("spot must be > 0")
    if strike <= 0.0:
        raise ValueError("strike must be > 0")
    if volatility <= 0.0:
        raise ValueError("volatility must be > 0")
    if time_to_maturity <= 0.0:
        raise ValueError("time_to_maturity must be > 0")
    if num_steps <= 0:
        raise ValueError("num_steps must be > 0")

    dt = time_to_maturity / num_steps
    up = math.exp(volatility * math.sqrt(dt))
    down = 1.0 / up
    discount = math.exp(-rate * dt)
    growth = math.exp(rate * dt)
    risk_neutral_prob = (growth - down) / (up - down)

    if not 0.0 < risk_neutral_prob < 1.0:
        raise ValueError("risk-neutral probability must lie strictly between 0 and 1")

    call_values = []
    put_values = []
    for num_down_moves in range(num_steps + 1):
        terminal_price = spot * (up ** (num_steps - num_down_moves)) * (down ** num_down_moves)
        call_values.append(max(terminal_price - strike, 0.0))
        put_values.append(max(strike - terminal_price, 0.0))

    for step in range(num_steps - 1, -1, -1):
        call_values = [
            discount * (
                risk_neutral_prob * call_values[node]
                + (1.0 - risk_neutral_prob) * call_values[node + 1]
            )
            for node in range(step + 1)
        ]
        put_values = [
            discount * (
                risk_neutral_prob * put_values[node]
                + (1.0 - risk_neutral_prob) * put_values[node + 1]
            )
            for node in range(step + 1)
        ]

    return call_values[0], put_values[0]


def run_example() -> None:
    """Price one call and one put with a binomial tree and verify put-call parity."""
    spot = 100.0
    strike = 100.0
    rate = 0.05
    volatility = 0.20
    time_to_maturity = 1.0
    num_steps = 10000

    call_price, put_price = binomial_tree_option_prices(
        spot,
        strike,
        rate,
        volatility,
        time_to_maturity,
        num_steps,
    )

    parity_left = call_price - put_price
    parity_right = spot - strike * math.exp(-rate * time_to_maturity)
    parity_error = abs(parity_left - parity_right)

    print(f"num_steps:  {num_steps}")
    print(f"call price: {call_price:.10f}")
    print(f"put price:  {put_price:.10f}")
    print(f"parity lhs: {parity_left:.10f}")
    print(f"parity rhs: {parity_right:.10f}")
    print(f"abs error:  {parity_error:.10e}")

    tolerance = 1.0e-10
    if parity_error <= tolerance:
        print("put-call parity check passed")
    else:
        raise AssertionError("put-call parity check failed")


if __name__ == "__main__":
    run_example()

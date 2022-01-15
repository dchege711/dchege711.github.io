#include <iostream>
#include <cassert>

enum class Month {
    Jan = 1, Feb, Mar, Apr, May, June, July, Aug, Sept, Oct, Nov, Dec};

/**
 * Core idea: Index the days in an increasing manner. 01/01/1900 corresponds
 * to 1, 02/01/1900 corresponds to 32, and so forth.
 */

/**
 * Return the number of days in month `increasing_month_idx`, where a value of
 * 1 corresponds to Jan 1900, and values increase monotonically thereafter.
 */
int NumOfDaysInMonth(int increasing_month_idx) {
    assert(increasing_month_idx > 0);

    int month_idx_base12 = increasing_month_idx % 12;
    if (month_idx_base12 == 0) month_idx_base12 = 12;

    Month month = static_cast<Month>(month_idx_base12);
    assert(month >= Month::Jan && month <= Month::Dec);

    switch (month) {
        case Month::Sept:
        case Month::Apr:
        case Month::June:
        case Month::Nov:
            return 30;

        case Month::Feb: {
            const int year = 1900 + (increasing_month_idx / 12);
            const bool is_leap_year =
                (year % 100 == 0) ? (year % 400 == 0) : (year % 4 == 0);
            return is_leap_year ? 29 : 28;
        }

        default:
            return 31;
    }
}

/**
 * Given that 1 Jan 1900 was a Monday, how many Sundays fell on the first of the
 * month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 *
 * [1]: https://projecteuler.net/problem=19
 */
void solve_euler_19() {
    // 365 for the year 1900, and (365.25 * 100) for the next 100 years.
    const int total_num_days = 365 + 36525;

    // Returns true if `day_idx` is between 1 Jan 1901 and 31 Dec 200,
    // inclusive.
    std::function<bool(int)> is_in_range_of_interest = [&](int day_idx) {
        return day_idx > 365 && day_idx <= total_num_days;
    };

    // Returns true if `day_idx` was a Sunday. Uses the fact that 7th Jan, 1900
    // was a Sunday.
    std::function<bool(int)> is_a_sunday = [&](int day_idx) {
        return day_idx % 7 == 0;
    };

    int num_sundays_on_first_of_the_month = 0;
    int increasing_month_idx = 1;
    int day_idx = 0;
    while (day_idx <= total_num_days) {
        int first_day_of_month_idx = day_idx + 1;
        if (is_a_sunday(first_day_of_month_idx)
            && is_in_range_of_interest(first_day_of_month_idx)) {
                num_sundays_on_first_of_the_month += 1;
        }
        day_idx += NumOfDaysInMonth(increasing_month_idx);
        increasing_month_idx += 1;
    }

    std::cout << num_sundays_on_first_of_the_month << " Sundays\n";
}

int main() {
    solve_euler_19();
    return 0;
}

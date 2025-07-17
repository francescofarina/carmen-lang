use carmen_lang::common::utils::{gcd, lcm};

#[cfg(test)]
mod utils_tests {
    use super::*;

    #[test]
    fn test_gcd() {
        assert_eq!(gcd(10, 25), 5);
        assert_eq!(gcd(14, 15), 1);
        assert_eq!(gcd(0, 5), 5);
        assert_eq!(gcd(5, 0), 5);
        assert_eq!(gcd(1, 1), 1);
    }

    #[test]
    fn test_lcm() {
        assert_eq!(lcm(4, 6), 12);
        assert_eq!(lcm(7, 5), 35);
        assert_eq!(lcm(1, 10), 10);
    }
}

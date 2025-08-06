use repetitive::repetitive;

fn main() {
    repetitive! {
        @for i in 0..10 {
            @let even = @{ println!("{} is even", @i); };
            @let odd = @{ println!("{} is odd", @i); };

            @if i % 2 == 0 {
                @even
            } else {
                @odd
            }
        }
    }
}

use support::{decl_storage, decl_module, StorageValue, StorageMap,
    dispatch::Result, ensure, decl_event, traits::Currency};
use system::ensure_signed;
use runtime_primitives::traits::{As, Hash, Zero};
use support::storage_items;
use rstd::prelude::*;
use runtime_io::print;
// use substrate_primitives::U256;
use parity_codec::{Encode, Decode};
// use runtime_io::with_storage;

const CHUNK_SIDE: usize = 8;
const FIELD_SIDE: usize = 8;

type Color = [u8; 3];

#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct Pixel<Balance> {
    price: Balance,
    color: Color, //rgb
}

pub trait Trait: balances::Trait {
    type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

decl_event!(
    pub enum Event<T>
    where
        <T as system::Trait>::AccountId,
        <T as balances::Trait>::Balance
    {
        Bought(AccountId, u16, u16, Balance),
    }
);

decl_storage! {
    trait Store for Module<T: Trait> as PlaceStorage {

        OwnedPixelArray get(pixel_of_owner_by_index): map (T::AccountId, u64) => (u16, u16);
        OwnedPixelCount get(owned_pixel_count): map T::AccountId => u64;
        PixelOwner get(owner_of): map (u16, u16) => Option<T::AccountId>;
        
        Chunks get(chunks) build(|_config: &GenesisConfig<T>| {
            let empty = Pixel {
                price: <T::Balance as As<u64>>::sa(1),
                color: [0,0,0]
            };
			(0..FIELD_SIDE)
				.map(|i| (
					i as u16,
					vec![empty.clone(); CHUNK_SIDE*CHUNK_SIDE]
				))
				.collect::<Vec<_>>()
		}): map u16 => Vec<Pixel<T::Balance>>;

    }
}

decl_module! {
    pub struct Module<T: Trait> for enum Call where origin: T::Origin {

        fn deposit_event<T>() = default;

        fn purchase_pixel(origin, x: u16, y: u16, color: Color, new_price: T::Balance) -> Result {
            let sender = ensure_signed(origin)?;

            let (chunk_num, index) = from_cartesian(x,y);
            print(chunk_num as u64);

            let owned_pixel_count = Self::owned_pixel_count(&sender);
            let new_owned_pixel_count = owned_pixel_count.checked_add(1).ok_or("Overflow buying new pixel for user")?;

            <OwnedPixelArray<T>>::insert((sender.clone(), owned_pixel_count), (x,y));
            <OwnedPixelCount<T>>::insert(&sender, new_owned_pixel_count);
            <PixelOwner<T>>::insert((x,y), &sender);

            //TODO price check
            let new_pixel = Pixel {
                price: new_price,
                color
            };

            <Chunks<T>>::mutate(chunk_num, |chunk| chunk[index as usize] = new_pixel);

            Self::deposit_event(RawEvent::Bought(sender, x, y, new_price));

            Ok(())
        }

        /// Initialize method for root to create empty field of specified size 
        /// TODO remove origin
        pub fn initialize_field(origin, field_size: u16) -> Result {
            let empty = Pixel {
                price: <T::Balance as As<u64>>::sa(1),
                color: [1,1,1]
            };
            for i in 0..field_size*field_size {
                let chunk = vec![empty.clone(); CHUNK_SIDE*CHUNK_SIDE];
                <Chunks<T>>::insert(i, chunk);
            }
            Ok(())
        }

        fn create_fixed_pixel(origin,  x: u16, y: u16, price: T::Balance) -> Result {
            Self::purchase_pixel(origin, x, y, [1,2,3], <T::Balance as As<u64>>::sa(1))
        }

        // fn obtain_region(x1: u16, y1: u16, x2: u16, y2: u16) -> Result<Color, &'static str> {
        //     Ok([1,2,3])
        // }
    
    }
}

impl<T: Trait> Module<T> {
}

fn from_cartesian(x: u16, y: u16) -> (u16,u16) {
    let chunk_x = x / 8;
    let chunk_y = y / 8;
    let chunk_number = chunk_x + chunk_y * FIELD_SIDE as u16;
    
    let local_x = x % 8;
    let local_y = y % 8;
    let index = local_x + local_y * CHUNK_SIDE as u16;

    (chunk_number, index)
}

fn to_cartesian(x: u16, y: u16) -> (u16,u16) {
    (2,3)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn cartesian_conversion_first_chunk() {
        assert_eq!(from_cartesian(0,0), (0,0));
        assert_eq!(from_cartesian(7,0), (0,7));
        assert_eq!(from_cartesian(0,1), (0,8));
        assert_eq!(from_cartesian(1,1), (0,9));
        assert_eq!(from_cartesian(0,2), (0,16));
    }

    #[test]
    fn cartesian_conversion_chunk_numbers() {
        assert_eq!(from_cartesian(8,0), (1,0));
        assert_eq!(from_cartesian(9,0), (1,1));
        assert_eq!(from_cartesian(8,1), (1,8));
        assert_eq!(from_cartesian(15,0), (1,7));

        assert_eq!(from_cartesian(0,8), (8,0));
        assert_eq!(from_cartesian(0,9), (8,8));

        assert_eq!(from_cartesian(8,8), (9,0));
        assert_eq!(from_cartesian(16,16), (18,0));

        assert_eq!(from_cartesian(56,0), (7,0));
    }

}
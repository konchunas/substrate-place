use support::{decl_storage, decl_module, StorageValue, StorageMap,
    dispatch::Result, ensure, decl_event, traits::Currency};
use system::ensure_signed;
use runtime_primitives::traits::{As, Hash, Zero};
use support::storage_items;
use rstd::prelude::*;
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
    // x: u16,
    // y: u16,
}

pub trait Trait: balances::Trait {
    type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

decl_event!(
    pub enum Event<T>
    where
        <T as system::Trait>::AccountId,
        <T as system::Trait>::Hash,
        <T as balances::Trait>::Balance
    {
        Bought(AccountId, Hash, Balance),
    }
);

decl_storage! {
    trait Store for Module<T: Trait> as PlaceStorage {

        OwnedPixelArray get(pixel_of_owner_by_index): map (T::AccountId, u64) => T::Hash;
        OwnedPixelCount get(owned_pixel_count): map T::AccountId => u64;
        OwnedPixelIndex: map T::Hash => u64;

        Field: linked_map (u16, u16) => T::Hash;
        Pixels get(pixel): map T::Hash => Pixel<T::Balance>;
        PixelOwner get(owner_of): map T::Hash => Option<T::AccountId>;
        
        // Chunk: [u16; 255];

        // Chunks: map u16 => (linked_map (u16) => Pixel<T::Balance>);
        // Chunks: map u16 => Vec<Pixel<T::Balance>>;

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

        Chonky get(chonky) build(|config| vec![(0, vec![1u8; 3]), (3, vec![5u8; 4])]): map u16 => Vec<u8>;

        PlainValue build(|config| 13): u32;
    }
    
    // add_extra_genesis {
	// 	config(amount): u32;
	// 	build(|storage: &mut runtime_primitives::StorageOverlay, _: &mut runtime_primitives::ChildrenStorageOverlay, config: &GenesisConfig<T>| {
	// 		with_storage(storage, || {
	// 			for i in 0..config.amount {
    //                 <Field<T>>::insert((i,i), i);
	// 			}
	// 		});
	// 	});
	// }
}

decl_module! {
    pub struct Module<T: Trait> for enum Call where origin: T::Origin {

        fn deposit_event<T>() = default;

        fn purchase_pixel(origin, x: u16, y: u16, color: Color, new_price: T::Balance) -> Result {
            let sender = ensure_signed(origin)?;
            let pixel_id = (x,y).using_encoded(<T as system::Trait>::Hashing::hash);

            let new_pixel = Pixel {
                // x,
                // y,
                price: new_price,
                color
            };

            // let owned_pixel_count = Self::owned_pixel_count(&sender);
            // let new_owned_pixel_count = owned_pixel_count.checked_add(1).ok_or("Overflow adding new pixel")?;

            // <OwnedPixelArray<T>>::insert((sender.clone(), owned_pixel_count), pixel_id);
            // <OwnedPixelCount<T>>::insert(&sender, new_owned_pixel_count);
            // <OwnedPixelIndex<T>>::insert(pixel_id, owned_pixel_count);

            // <Pixels<T>>::insert(pixel_id, new_pixel);
            // <PixelOwner<T>>::insert(pixel_id, &sender);
            <Field<T>>::insert((x,y), pixel_id);

            Self::deposit_event(RawEvent::Bought(sender, pixel_id, new_price));

            Ok(())
        }

        // fn create_a_bunch_of_pixels(origin, amount: u16) -> Result {
        //     for i in 0..amount {
        //         let hash = (i,i).using_encoded(<T as system::Trait>::Hashing::hash);
        //         <Field<T>>::insert((i,i), hash);
        //     }
        //     let dummy = Pixel {
        //         price: <T::Balance as As<u64>>::sa(1),
        //         color: [1,2,3]
        //     };
        //     let chunk = vec![dummy; 64];
        //     // chunk[1] = 3u8;
        //     <Chunks<T>>::insert(0, chunk);
            
        //     let dummy = Pixel {
        //         price: <T::Balance as As<u64>>::sa(5),
        //         color: [2,6,7]
        //     };
        //     let chunk = vec![dummy; 64];
        //     <Chunks<T>>::insert(1, chunk);
        //     Ok(())
        // }

        /// Initialize method for root to create empty field of specified size 
        pub fn initialize_field(origin, field_size: u16) -> Result {
            let empty = Pixel {
                price: <T::Balance as As<u64>>::sa(1),
                color: [1,1,1]
            };
            for i in 0..field_size {
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
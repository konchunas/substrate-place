//! # The Place on Substrate
//!
//! The Place module provides functionality for huge pixel drawing board on Substrate
//!
//! ## Overview
//!
//! The place module provides functions for:
//! - Getting board state
//! - Purchasing particular pixel
//!
//!  ### Terminology
//!
//! **Pixel:** single smallest unit of the board. Stores price and color of a dot on a board.
//! 
//! **Chunk:** Matrix of pixels (currently 8x8). Stored in chunks to optimize retreival by user
//! 
//! **Initialized chunk:** Chunk which has been filled with minimal price default color pixels. Happens when any chunk pixel is bought first time.
//! 
//! **Absolute** or **Pixel coordinates**: Coordinates on the grid without any relation to chunk whatsoever. Plain pixel address.
//! 
//! **Chunk coordinates**: First coordinate of 8x8 pixel group
//!
//! ### Storage structure
//!
//! Pixel is structure holding color and price. Color is array of 3 one-byte values each one representing color component: red, green and blue.
//! Every pixel is stored in so-called chunks. Logically chunks are 8x8 pixel matrices. To allow batch retreiving chunks are stored in 64-element Vectors.
//! Every chunk on map is accessed by (i32, i32) coordinates. These coordinates can be both positive and negative.
//! This practically means there can be 2^32 * 2^32 chunks. Since every chunk contains 8 pixel per axes it means we can have to contain 2^3 more pixels per chunk resulting in 2^35 * 2^35 pixel board.
//!
//! ### Usage
//! 
//! To retreive latest state of the board `chunks` call is used. Pass desired chunk coord as tuple as input argument.
//! 64-element array will be returned as an output if this chunk was already initialized, otherwise returning empty array
//! 
//! When user wants to buy a pixel on the board `purchase_pixel` method should be called.
//! Its arguments are as follows:
//!  - x, y - absolute coordinates of the pixel
//!  - color - three-byte array with RGB coded desired color of a pixel
//!  - new_price - amount of funds user pays for this pixel
//! 
//! ### Dependencies
//! 
//! This module is reliant on Sudo module.
//! It is needed to reward sudo user when first pixel in a chunk is bought.

use rstd::prelude::*;

use parity_codec::{Decode, Encode};
use runtime_io::print;
use runtime_primitives::traits::As;
use support::{
    decl_event, decl_module, decl_storage,
    ensure,
    dispatch::Result, traits::Currency, StorageMap,
};
use system::ensure_signed;

/// Represents how many pixels per axis there are in chunk
const CHUNK_SIDE: usize = 8;

/// Default price for never purchased before pixel
const DEFAULT_PRICE: u64 = 1;

//2^31 for chunks in chunks in negative and positive direction
//2^3 inside chunk per axis
const MIN_COORD: i64 = -1 << 34; //2^31 + 2^3
const MAX_COORD: i64 = (1 << 34) - 1; //2^31 + 2^3

/// Represents RGB values. 3 channels 1 byte per color
type Color = [u8; 3];


/// Basic element of the grid, stored as Color and Price
#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct Pixel<Balance> {
    price: Balance,
    color: Color, //rgb
}

pub trait Trait: balances::Trait + sudo::Trait {
    type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

decl_event!(
    pub enum Event<T>
    where
        <T as system::Trait>::AccountId,
        <T as balances::Trait>::Balance
    {
        //Emitted when new pixel is bought to show who bought it and what was the price
        Bought(AccountId, i64, i64, Balance),
    }
);


decl_storage! {
    trait Store for Module<T: Trait> as PlaceStorage {

        /// Whole field is stored as 8x8 chunks and those chunks are stored in i32xi32 map
        /// to store absolute pixel coordinates we would need i35 since there is 8 more coordinates per chunk
        /// i64 is the closest we have to i35 for storing absolute pixel coordinates
        
        /// Stores list of pixels owned by specific account
        OwnedPixelArray get(pixel_of_owner_by_index): map (T::AccountId, u64) => (i64, i64);
        /// Length of list of user-owned pixels
        OwnedPixelCount get(owned_pixel_count): map T::AccountId => u64;
        /// Owner of pixel by absolute coordinates
        PixelOwner get(owner_of): map (i64, i64) => Option<T::AccountId>;

        /// Main storage of the grid. Organized as i32xi32 map of pixel Vectors
        Chunks get(chunks): map (i32, i32) => Vec<Pixel<T::Balance>>;
    }
}

decl_module! {
    pub struct Module<T: Trait> for enum Call where origin: T::Origin {

        fn deposit_event<T>() = default;

        /// Method to purchase specific pixel on the grid. Pixel is specified using `x` and `y` using absolute coordinates.
        /// `color` is RGB 3-byte array. `price` is of Balance type.
        /// Price should be neccessary larger than previous price.
        /// Chunk coords of the pixel are calculated automatically.
        /// Caclculated chunk will be initialized if no pixel was ever bought on it.
        fn purchase_pixel(origin, x: i64, y: i64, color: Color, new_price: T::Balance) -> Result {
            let sender = ensure_signed(origin)?;

            ensure!(MIN_COORD <= x && x <= MAX_COORD, "X is out of bounds");
            ensure!(MIN_COORD <= y && y <= MAX_COORD, "Y is out of bounds");

            /// convert pixel to chunk coordinates
            let ((chunk_x, chunk_y), pixel_index) = from_absolute(x,y);

            /// extract pixel previous owner and price
            let chunk_exists = <Chunks<T>>::exists((chunk_x, chunk_y));
            let (old_price, prev_owner) = if !chunk_exists {
                let default_price = <T::Balance as As<u64>>::sa(DEFAULT_PRICE);
                (default_price, None)    
            } else {
                let chunk = <Chunks<T>>::get((chunk_x, chunk_y));
                let pixel = &chunk[pixel_index as usize];
                let owner = Self::owner_of((x,y));

                (pixel.price, owner)  
            };
            
            ensure!(new_price > old_price, "Price is too low");

            /// if there were no owner of this pixel it means Sudo user is eligible for payment
            let refund_recipient = match prev_owner {
                Some(ref prev_owner) => prev_owner.clone(),
                None => <sudo::Module<T>>::key()
            };

            Self::transfer_ownership(x, y, prev_owner, sender.clone())?;

            //it should be safe to mutate storage from now on

            <balances::Module<T> as Currency<_>>::transfer(&sender, &refund_recipient, new_price)?;

            if !chunk_exists { 
                Self::init_chunk(chunk_x, chunk_y)?;
            }

            let new_pixel = Pixel {
                price: new_price,
                color
            };

            /// mutate single value in vector of pixels in calculated pixel index inside chunk vector
            <Chunks<T>>::mutate((chunk_x, chunk_y), |chunk| chunk[pixel_index as usize] = new_pixel);

            Self::deposit_event(RawEvent::Bought(sender, x, y, new_price));

            Ok(())
        }

        /// Testnet only method emulating faucet behaviour
        /// Since Sudo user has all the funds we can grant somet to people for testing purposes
        fn use_faucet(origin) {
            //TODO limit to testnet-only
            let receiver = ensure_signed(origin)?;
            let amount = <T::Balance as As<u64>>::sa(1000);
            let superuser = <sudo::Module<T>>::key();
            <balances::Module<T> as Currency<_>>::transfer(&superuser, &receiver, amount)?;   
        }
    }
}

impl<T: Trait> Module<T> {
    ///  Initialize chunk at specific coordinates and set all of its pixels to be white color and of `DEFAULT_PRICE`
    fn init_chunk(x: i32, y: i32) -> Result {
        let empty_pixel = Pixel {
            price: <T::Balance as As<u64>>::sa(DEFAULT_PRICE),
            color: [255, 255, 255],                            //white
        };
        <Chunks<T>>::insert((x, y), vec![empty_pixel.clone(); CHUNK_SIDE * CHUNK_SIDE]);
        Ok(())
    }

    /// Transfer pixel ownership from previous to new owner
    /// Removes pixel from owned pixels array and decreases owned pixels count
    fn transfer_ownership(x: i64, y: i64, prev_owner: Option<T::AccountId>, new_owner: T::AccountId) -> Result {
        if let Some(prev_owner) = prev_owner {
            let prev_owner_pixel_count = Self::owned_pixel_count(&prev_owner);
            let new_pixel_count = prev_owner_pixel_count.checked_sub(1).ok_or("Overflow removing old pixel from user")?;
            <OwnedPixelArray<T>>::remove((prev_owner.clone(), new_pixel_count));
            <OwnedPixelCount<T>>::insert(&prev_owner, new_pixel_count);
        }

        let owned_pixel_count = Self::owned_pixel_count(&new_owner);
        let new_owned_pixel_count = owned_pixel_count.checked_add(1).ok_or("Overflow buying new pixel for user")?;

        <OwnedPixelArray<T>>::insert((new_owner.clone(), owned_pixel_count), (x,y));
        <OwnedPixelCount<T>>::insert(&new_owner, new_owned_pixel_count);
        <PixelOwner<T>>::insert((x,y), &new_owner); // will overwrite previous owner if exists
        Ok(())
    }
}

/// Convert absolute pixel coordinates to chunk coordinates and pixel index inside that chunk
/// Returns tuple containing nested tuple with chunk `x` and `y` coordinates and index of a pixel
fn from_absolute(x: i64, y: i64) -> ((i32, i32), u8) {
    let (chunk_x, local_x) = convert_coord(x);
    let (chunk_y, local_y) = convert_coord(y);

    // convert pixel coordinate from virtual 8x8 matrix to vector index
    let index = local_x as u8 + local_y as u8 * CHUNK_SIDE as u8;

    ((chunk_x, chunk_y), index)
}

// Convert singular axis coordinate from absolute to chunk
// Returns tuple of chunk coordinate and pixel coordinate on virtual 8x8 Matrix
fn convert_coord(c: i64) -> (i32, u8) {
    let chunk = c.div_euclid(CHUNK_SIDE as i64);
    let local = c.rem_euclid(CHUNK_SIDE as i64);
    (chunk as i32, local as u8)
}

#[cfg(test)]
mod tests {
    use super::*;

    use primitives::{Blake2Hasher, H256};
    use runtime_io::{with_externalities, TestExternalities};
    use runtime_primitives::{
        testing::{Digest, DigestItem, Header},
        traits::{BlakeTwo256, IdentityLookup},
        BuildStorage,
    };
    use support::{assert_ok, impl_outer_dispatch, impl_outer_origin};

    impl_outer_origin! {
        pub enum Origin for PlaceTest {}
    }

    impl_outer_dispatch! {
        pub enum Call for PlaceTest where origin: Origin {
            balances::Balances,
        }
    }

    #[derive(Clone, Eq, PartialEq)]
    pub struct PlaceTest;

    impl system::Trait for PlaceTest {
        type Origin = Origin;
        type Index = u64;
        type BlockNumber = u64;
        type Hash = H256;
        type Hashing = BlakeTwo256;
        type Digest = Digest;
        type AccountId = u64;
        type Lookup = IdentityLookup<Self::AccountId>;
        type Header = Header;
        type Event = ();
        type Log = DigestItem;
    }

    impl balances::Trait for PlaceTest {
        type Balance = u64;
        type OnFreeBalanceZero = ();
        type OnNewAccount = ();
        type Event = ();
        type TransactionPayment = ();
        type TransferPayment = ();
        type DustRemoval = ();
    }

    impl sudo::Trait for PlaceTest {
        type Proposal = Call;
        type Event = ();
    }

    impl super::Trait for PlaceTest {
        type Event = ();
    }

    type Place = super::Module<PlaceTest>;
    type Balances = balances::Module<PlaceTest>;
    type Sudo = sudo::Module<PlaceTest>;

    fn build_ext() -> TestExternalities<Blake2Hasher> {
        let mut t = system::GenesisConfig::<PlaceTest>::default()
            .build_storage()
            .unwrap()
            .0;
        t.extend(
            balances::GenesisConfig::<PlaceTest> {
                balances: vec![(1, 10), (2, 20), (3, 30), (4, 40), (5, 50), (6, 60)],
                ..Default::default()
            }
            .build_storage()
            .unwrap()
            .0,
        );
        t.into()
    }

    #[test]
    fn purchase_pixel_should_work() {
        with_externalities(&mut build_ext(), || {
            assert_ok!(Place::purchase_pixel(Origin::signed(1), 0, 0, [1, 2, 3], 3));

            let chunk = Place::chunks((0, 0));
            assert_eq!(chunk[0].price, 3);
        })
    }

    #[test]
    fn purchase_negative_chunk_should_work() {
        with_externalities(&mut build_ext(), || {
            assert_ok!(Place::purchase_pixel(
                Origin::signed(1),
                -1,
                -1,
                [1, 2, 3],
                3
            ));

            let chunk = Place::chunks((-1, -1));
            assert_eq!(chunk[63].price, 3);
        })
    }

    #[test]
    fn init_negative_chunk_should_work() {
        with_externalities(&mut build_ext(), || {
            assert_ok!(Place::init_chunk(-1, -1));

            let chunk = Place::chunks((-1, -1));
            assert!(!chunk.is_empty());

            assert_eq!(chunk[0].price, 1);
        })
    }
    
    #[test]
    fn ownership_transfer_should_work() {
        with_externalities(&mut build_ext(), || {
            // check no one owns the pixel
            assert_eq!(Place::owner_of((1, 1)), None);
            // user purchases it
            assert_ok!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 2));
            // check user is owner now
            assert_eq!(Place::owner_of((1, 1)), Some(1));
            assert_eq!(Place::owned_pixel_count(1), 1);
            // first pixel of 10th user is at (1,1)
            assert_eq!(Place::pixel_of_owner_by_index((1, 0)), (1, 1));

            //check that another user isn't owning anything
            assert_eq!(Place::owned_pixel_count(5), 0);

            // another user purchases the same pixel
            assert_ok!(Place::purchase_pixel(Origin::signed(5), 1, 1, [1, 2, 3], 3));
            // check another user is owner now
            assert_eq!(Place::owner_of((1, 1)), Some(5));
            assert_eq!(Place::owned_pixel_count(5), 1);
            assert_eq!(Place::pixel_of_owner_by_index((5, 0)), (1, 1));
            // check that first user isn't owning anything now
            assert_eq!(Place::owned_pixel_count(1), 0);
        })
    }

    #[test]
    fn cant_buy_not_enough_funds() {
        with_externalities(&mut build_ext(), || {
            // this fails since 127th user has no money
            assert!(Place::purchase_pixel(Origin::signed(127), 1, 1, [1, 2, 3], 1).is_err());
        });
    }
    #[test]
    fn sudo_receives_funds() {
        with_externalities(&mut build_ext(), || {
            assert_eq!(Place::owner_of((1, 1)), None);
            assert_ok!(Place::purchase_pixel(
                Origin::signed(1),
                1,
                1,
                [1, 2, 3],
                3
            ));

            // check that we received first payment from uninitialized chunk
            assert_eq!(Balances::free_balance(Sudo::key()), 3);

            assert_ok!(Place::purchase_pixel(
                Origin::signed(1),
                2,
                2,
                [1, 2, 3],
                4
            ));
            // check that we received second payment from already initialized chunk
            assert_eq!(Balances::free_balance(Sudo::key()), 7);
        });
    }

    #[test]
    fn should_refund_pixel() {
        with_externalities(&mut build_ext(), || {
            //user purchases pixel
            assert_ok!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 10));
            // another user purchases the same pixel paying more
            assert_ok!(Place::purchase_pixel(Origin::signed(2), 1, 1, [1, 2, 3], 15));
            // first user receives its funds back
            assert_eq!(Balances::free_balance(1), 15);
        });
    }
    
    #[test]
    fn pixel_refund_price_is_low() {
        with_externalities(&mut build_ext(), || {
            //user purchases pixel
            assert_ok!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 10));
            // another user fails to purchase the same pixel paying less
            assert!(Place::purchase_pixel(Origin::signed(2), 1, 1, [1, 2, 3], 5).is_err());
        });
    }

    #[test]
    fn pixel_refund_not_enough_funds() {
        with_externalities(&mut build_ext(), || {
            //user purchases pixel
            assert_ok!(Place::purchase_pixel(Origin::signed(2), 1, 1, [1, 2, 3], 20));
            // another user fails to purchase the same pixel not having enough funds
            assert!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 25).is_err());
        });
    }
    #[test]
    fn user_can_buy_from_himself() {
        with_externalities(&mut build_ext(), || {
            //user purchases pixel
            assert_ok!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 3));
            // same user fails to buy with the same price
            assert!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 3).is_err());
            // same user fails to buy with lesser price
            assert!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 2).is_err());
            // same user succeeds buying with greater price
            assert_ok!(Place::purchase_pixel(Origin::signed(1), 1, 1, [1, 2, 3], 4));
        });
    }

    #[test]
    fn can_purchase_last_pixel() {
        with_externalities(&mut build_ext(), || {
        
            assert_ok!(Place::purchase_pixel(Origin::signed(1), MAX_COORD, MAX_COORD, [1, 2, 3], 3));
            assert_ok!(Place::purchase_pixel(Origin::signed(1), MIN_COORD, MIN_COORD, [1, 2, 3], 6));
            
            let min_chunk_coord = std::i32::MIN;
            let max_chunk_coord = std::i32::MAX;
            
            let max_chunk = Place::chunks((max_chunk_coord, max_chunk_coord));
            let min_chunk = Place::chunks((min_chunk_coord, min_chunk_coord));
            assert_eq!(max_chunk[63].price, 3);
            assert_eq!(min_chunk[0].price, 6);
        });
    }

    #[test]
    fn purchase_off_limits_should_fail() {
        with_externalities(&mut build_ext(), || {
            assert!(Place::purchase_pixel(Origin::signed(1), MAX_COORD + 1, MAX_COORD + 1, [1, 2, 3], 3).is_err());
            assert!(Place::purchase_pixel(Origin::signed(1), MIN_COORD - 1, MIN_COORD - 1, [1, 2, 3], 6).is_err());
        });
    }

    #[test]
    fn max_coords_convert() {
        let min_chunk = std::i32::MIN;
        let max_chunk = std::i32::MAX;
        assert_eq!(from_absolute(MAX_COORD, MAX_COORD), ((max_chunk, max_chunk), 63));
        assert_eq!(from_absolute(MIN_COORD, MIN_COORD), ((min_chunk, min_chunk), 0));
    }

    #[test]
    fn pixel_convertsion_from_absolute() {
        assert_eq!(from_absolute(0, 0), ((0, 0), 0));
        assert_eq!(from_absolute(1, 0), ((0, 0), 1));
        assert_eq!(from_absolute(0, 1), ((0, 0), 8));
        assert_eq!(from_absolute(1, 1), ((0, 0), 9));
        assert_eq!(from_absolute(0, 2), ((0, 0), 16));
    }

    #[test]
    fn chunk_convertsion_from_absolute() {
        assert_eq!(from_absolute(0, 0), ((0, 0), 0));
        assert_eq!(from_absolute(8, 0), ((1, 0), 0));
        assert_eq!(from_absolute(0, 8), ((0, 1), 0));
    }

    #[test]
    fn negative_convertsion_from_absolute() {
        assert_eq!(from_absolute(-1, -1), ((-1, -1), 63));
        assert_eq!(from_absolute(-8, -8), ((-1, -1), 0));
        assert_eq!(from_absolute(-9, -9), ((-2, -2), 63));
    }
}

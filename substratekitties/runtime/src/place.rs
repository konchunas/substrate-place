use rstd::prelude::*;

use runtime_io::print;
use runtime_primitives::traits::As;
use support::{
    decl_event, decl_module, decl_storage, dispatch::Result, traits::Currency, StorageMap,
    StorageValue,
};
use system::ensure_signed;
// use substrate_primitives::U256;
use parity_codec::{Decode, Encode};
// use runtime_io::with_storage;

const CHUNK_SIDE: usize = 8;
const DEFAULT_PRICE: u64 = 1;

type Color = [u8; 3];

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
        Bought(AccountId, i64, i64, Balance),
    }
);

decl_storage! {
    trait Store for Module<T: Trait> as PlaceStorage {
        /// whole field is stored as 8x8 chunks and chunks are stored in i32xi32 map
        /// to store absolute pixel coordinates we would need i36 since there is 8 more coordinates per chunk
        /// but i64 is the closest we have to i36
        OwnedPixelArray get(pixel_of_owner_by_index): map (T::AccountId, u64) => (i64, i64);
        OwnedPixelCount get(owned_pixel_count): map T::AccountId => u64;
        PixelOwner get(owner_of): map (i64, i64) => Option<T::AccountId>;

        Chunks get(chunks): map (i32, i32) => Vec<Pixel<T::Balance>>;
    }
}

decl_module! {
    pub struct Module<T: Trait> for enum Call where origin: T::Origin {

        fn deposit_event<T>() = default;

        fn purchase_pixel(origin, x: i64, y: i64, color: Color, new_price: T::Balance) -> Result {
            let sender = ensure_signed(origin)?;

            print("Before from absolute");
            let ((chunk_x, chunk_y), index) = from_absolute(x,y);
            print("From absolute");

            let chunk_exists = <Chunks<T>>::exists((chunk_x, chunk_y));
            if !chunk_exists {
                let default_price = <T::Balance as As<u64>>::sa(DEFAULT_PRICE);
                if new_price < default_price {
                    return Err("Too low price for an empty chunk intialization")
                }
                let sudo_user = <sudo::Module<T>>::key();
                print("sending to sudo");
                <balances::Module<T> as Currency<_>>::transfer(&sender, &sudo_user, new_price)?;

                print("Init chunk");
                Self::init_chunk(chunk_x, chunk_y)?;
            } else {
                let prev_chunk = <Chunks<T>>::get((chunk_x, chunk_y));
                let prev_pixel = &prev_chunk[index as usize];
                let prev_owner = Self::owner_of((x,y)).ok_or("Pixel don't have and owner")?;

                if prev_pixel.price >= new_price {
                    return Err("New price should be greater than previous");
                }
                // If pixel had an owner we are going to return all the funds back to him and remove ownership
                <balances::Module<T> as Currency<_>>::transfer(&sender, &prev_owner, prev_pixel.price)?;

                let prev_owner_pixel_count = Self::owned_pixel_count(&prev_owner);
                    let new_pixel_count = prev_owner_pixel_count.checked_sub(1).ok_or("Overflow removing old pixel from user")?;
                <OwnedPixelArray<T>>::remove((prev_owner.clone(), new_pixel_count));
                <OwnedPixelCount<T>>::insert(&prev_owner, new_pixel_count);
            }


            let owned_pixel_count = Self::owned_pixel_count(&sender);
            let new_owned_pixel_count = owned_pixel_count.checked_add(1).ok_or("Overflow buying new pixel for user")?;

            <OwnedPixelArray<T>>::insert((sender.clone(), owned_pixel_count), (x,y));
            <OwnedPixelCount<T>>::insert(&sender, new_owned_pixel_count);
            <PixelOwner<T>>::insert((x,y), &sender); //will change overwrite previous owner if exists

            let new_pixel = Pixel {
                price: new_price,
                color
            };

            print("new pixel");
            <Chunks<T>>::mutate((chunk_x, chunk_y), |chunk| chunk[index as usize] = new_pixel);
            print("mutation complete");


            Self::deposit_event(RawEvent::Bought(sender, x, y, new_price));

            Ok(())
        }
            }
        }

impl<T: Trait> Module<T> {
    fn init_chunk(x: i32, y: i32) -> Result {
        let empty_pixel = Pixel {
            price: <T::Balance as As<u64>>::sa(DEFAULT_PRICE), //minimum price
            color: [255, 255, 255],                //white
        };
        <Chunks<T>>::insert((x, y), vec![empty_pixel.clone(); CHUNK_SIDE * CHUNK_SIDE]);
        Ok(())
    }
}

fn from_absolute(x: i64, y: i64) -> ((i32, i32), u8) {
    let (chunk_x, local_x) = convert_coord(x);
    let (chunk_y, local_y) = convert_coord(y);

    let index = local_x as u8 + local_y as u8 * CHUNK_SIDE as u8;

    ((chunk_x, chunk_y), index)
}

fn convert_coord(c: i64) -> (i32, u8) {
    let chunk = c.div_euclid(CHUNK_SIDE as i64);
    let local = c.rem_euclid(CHUNK_SIDE as i64);
    (chunk as i32, local as u8)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Import a bunch of dependencies from substrate core. All needed for some parts of the code.
    use primitives::{Blake2Hasher, H256};
    use runtime_io::{with_externalities, TestExternalities};
    use runtime_primitives::{
        testing::{Digest, DigestItem, Header},
        traits::{BlakeTwo256, IdentityLookup},
        BuildStorage,
    };
    use support::{assert_ok, impl_outer_origin, impl_outer_dispatch};

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
            assert_ok!(Place::purchase_pixel(
                Origin::signed(1),
                0,
                0,
                [1, 2, 3],
                3
            ));

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
    fn owner_can_transfer() {
        with_externalities(&mut build_ext(), || {
            assert_eq!(Place::owner_of((1, 1)), None);
            assert_ok!(Place::purchase_pixel(
                Origin::signed(1),
                1,
                1,
                [1, 2, 3],
                1
            ));
            assert_eq!(Place::owner_of((1, 1)), Some(1));
            assert_eq!(Place::owned_pixel_count(5), 0);
            assert_eq!(Place::owned_pixel_count(1), 1);
            // first pixel of 10th user is at (1,1)
            assert_eq!(Place::pixel_of_owner_by_index((1, 0)), (1, 1));

            // another user purchases the same pixel
            assert_ok!(Place::purchase_pixel(Origin::signed(5), 1, 1, [1, 2, 3], 2));
            assert_eq!(Place::owner_of((1, 1)), Some(5));
            assert_eq!(Place::owned_pixel_count(5), 1);
            assert_eq!(Place::owned_pixel_count(1), 0);
            // check that previous owner have received its funds back
            assert_eq!(Balances::free_balance(1), 10);


            println!("sudo balance {}", Balances::free_balance(Sudo::key()));
        })
    }

    #[test]
    fn cant_buy_not_enough_funds() {
        with_externalities(&mut build_ext(), || {
            // this fails since 127th user has no money
            assert!(Place::purchase_pixel(
                Origin::signed(127),
                1, 1,
                [1, 2, 3],
                1
            ).is_err());
        });
    }
    #[test]
    fn sudo_receives_funds() {
        with_externalities(&mut build_ext(), || {
            assert_eq!(Place::owner_of((1, 1)), None);
            assert_ok!(Place::purchase_pixel(
                Origin::signed(1),
                1, 1,
                [1, 2, 3],
                10
            ));

            // check that we received default pixel price
            assert_eq!(Balances::free_balance(Sudo::key()), 10);
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
            assert_eq!(Balances::free_balance(1), 10);
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
    fn user_buys_from_himself() {
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

module GlobalConst where

--
--- Threads and ticks
--

tickReal = 2 -- seconds
tickGame = 2 -- seconds

--
--- Navigation
--

const_jg_exit_radius = 10.0 -- how far from the jumpgate you end up after jumping
const_station_exit_radius = 10.0

--
--- Trade
--

const_pricing_buyToFair = 0.95  -- Buying  price is const * (fair price)
const_pricing_sellToFair = 1.05 -- Selling price is const * (fair price)

const_abundancy = 0.8 -- Don't really want to get more than const of stock of an item
const_deficit   = 0.2 -- Don't really want to have less than const of stock of  an item

MODULE GadRadPower
    USE Kinds
    USE conversions_frapcon
    USE Functions
    USE variables_frapcon, ONLY : ounit
    IMPLICIT NONE
    !>@brief
    !> Module GadRadPower contains the Gadolinia radial power profiles for LWR and HWR
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 12/9/2014
    !
    ! Flag for indicating whether to print warning message
    LOGICAL :: WarnMsg = .False.
    !
    TYPE GadPP
        INTEGER(ipk) :: NRadVals
        INTEGER(ipk) :: NBUVals
        INTEGER(ipk) :: NGdConcVals
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Radius
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: Burnup
        REAL(r8k), DIMENSION(:), ALLOCATABLE :: GadConc
        REAL(r8k), DIMENSION(:,:,:), ALLOCATABLE :: Power
    END TYPE GadPP
    !
    TYPE (GadPP), DIMENSION(:), ALLOCATABLE :: GadPow
    !
    ! Commons
    !
    INTEGER(ipk), PARAMETER :: NRadVals = 11
    INTEGER(ipk), PARAMETER :: NBUVals = 13
    INTEGER(ipk), PARAMETER :: NGdConcVals = 6
    !
    REAL(r8k), DIMENSION(NRadVals), PARAMETER, PRIVATE :: Gdrad = &
      &         (/         0.0_r8k, 0.123701138_r8k, 0.371103414_r8k, 0.556655121_r8k, &
      &            0.649430975_r8k, 0.711281544_r8k, 0.773132113_r8k, 0.834982682_r8k, &
      &            0.896833251_r8k, 0.946313706_r8k, 0.988619495_r8k /)
    ! burnup values
    REAL(r8k), DIMENSION(NBUVals), PARAMETER, PRIVATE :: GdBu = &
      &         (/  0.0_r8k,  0.5_r8k,  1.0_r8k,  1.5_r8k,  2.0_r8k, &
      &             3.0_r8k,  4.0_r8k,  5.0_r8k,  7.0_r8k, 10.0_r8k, &
      &            20.0_r8k, 40.0_r8k, 60.0_r8k /)
    ! gadolinia concentration, wt%
    REAL(r8k), DIMENSION(NGdConcVals), PARAMETER, PRIVATE :: GdConc = &
      &         (/ 0.0_r8k, 2.0_r8k, 4.0_r8k, 6.0_r8k, 8.0_r8k, 10.0_r8k /)
    !
    ! LWR Power Profile Values
    !
    REAL(r8k), DIMENSION(NGdConcVals,NRadVals,NBUVals), PARAMETER, PRIVATE :: gdPPLWR = reshape (&
      &         (/ 0.8879_r8k, 0.5807_r8k, 0.5649_r8k, 0.5657_r8k, 0.5717_r8k, 0.5797_r8k, &
      &            0.8888_r8k, 0.5813_r8k, 0.5654_r8k, 0.5663_r8k, 0.5723_r8k, 0.5803_r8k, &
      &            0.9024_r8k, 0.6142_r8k, 0.5949_r8k, 0.5932_r8k, 0.5970_r8k, 0.6030_r8k, &
      &            0.9211_r8k, 0.6669_r8k, 0.6420_r8k, 0.6361_r8k, 0.6365_r8k, 0.6395_r8k, &
      &            0.9344_r8k, 0.7136_r8k, 0.6841_r8k, 0.6750_r8k, 0.6726_r8k, 0.6733_r8k, &
      &            0.9456_r8k, 0.7613_r8k, 0.7276_r8k, 0.7158_r8k, 0.7111_r8k, 0.7097_r8k, &
      &            0.9594_r8k, 0.8265_r8k, 0.7874_r8k, 0.7721_r8k, 0.7644_r8k, 0.7604_r8k, &
      &            0.9741_r8k, 0.9295_r8k, 0.8829_r8k, 0.8623_r8k, 0.8504_r8k, 0.8426_r8k, &
      &            0.9928_r8k, 1.1074_r8k, 1.0570_r8k, 1.0277_r8k, 1.0087_r8k, 0.9948_r8k, &
      &            1.0117_r8k, 1.3802_r8k, 1.3605_r8k, 1.3283_r8k, 1.3006_r8k, 1.2778_r8k, &
      &            1.5819_r8k, 2.8384_r8k, 3.1334_r8k, 3.2577_r8k, 3.3147_r8k, 3.3389_r8k, &
      &            0.8862_r8k, 0.4631_r8k, 0.3922_r8k, 0.3679_r8k, 0.3575_r8k, 0.3532_r8k, &
      &            0.8871_r8k, 0.4636_r8k, 0.3925_r8k, 0.3683_r8k, 0.3579_r8k, 0.3535_r8k, &
      &            0.9010_r8k, 0.5192_r8k, 0.4319_r8k, 0.4007_r8k, 0.3860_r8k, 0.3787_r8k, &
      &            0.9201_r8k, 0.6225_r8k, 0.5043_r8k, 0.4588_r8k, 0.4356_r8k, 0.4224_r8k, &
      &            0.9338_r8k, 0.7254_r8k, 0.5809_r8k, 0.5194_r8k, 0.4869_r8k, 0.4674_r8k, &
      &            0.9452_r8k, 0.8298_r8k, 0.6715_r8k, 0.5914_r8k, 0.5476_r8k, 0.5206_r8k, &
      &            0.9593_r8k, 0.9659_r8k, 0.8179_r8k, 0.7126_r8k, 0.6490_r8k, 0.6086_r8k, &
      &            0.9744_r8k, 1.1330_r8k, 1.0626_r8k, 0.9510_r8k, 0.8588_r8k, 0.7914_r8k, &
      &            0.9936_r8k, 1.3200_r8k, 1.4077_r8k, 1.3846_r8k, 1.3183_r8k, 1.2421_r8k, &
      &            1.0132_r8k, 1.4818_r8k, 1.7301_r8k, 1.8716_r8k, 1.9523_r8k, 1.9921_r8k, &
      &            1.5862_r8k, 2.4757_r8k, 3.0083_r8k, 3.3738_r8k, 3.6502_r8k, 3.8700_r8k, &
      &            0.8869_r8k, 0.4746_r8k, 0.3561_r8k, 0.3194_r8k, 0.3028_r8k, 0.2944_r8k, &
      &            0.8878_r8k, 0.4751_r8k, 0.3564_r8k, 0.3197_r8k, 0.3031_r8k, 0.2947_r8k, &
      &            0.9015_r8k, 0.5603_r8k, 0.4085_r8k, 0.3589_r8k, 0.3356_r8k, 0.3229_r8k, &
      &            0.9204_r8k, 0.7151_r8k, 0.5164_r8k, 0.4369_r8k, 0.3982_r8k, 0.3759_r8k, &
      &            0.9339_r8k, 0.8469_r8k, 0.6440_r8k, 0.5288_r8k, 0.4699_r8k, 0.4356_r8k, &
      &            0.9451_r8k, 0.9509_r8k, 0.7939_r8k, 0.6501_r8k, 0.5643_r8k, 0.5128_r8k, &
      &            0.9590_r8k, 1.0598_r8k, 1.0019_r8k, 0.8648_r8k, 0.7449_r8k, 0.6608_r8k, &
      &            0.9739_r8k, 1.1590_r8k, 1.2359_r8k, 1.1982_r8k, 1.1034_r8k, 0.9999_r8k, &
      &            0.9929_r8k, 1.2571_r8k, 1.4489_r8k, 1.5521_r8k, 1.5931_r8k, 1.5858_r8k, &
      &            1.0124_r8k, 1.3375_r8k, 1.6016_r8k, 1.7887_r8k, 1.9299_r8k, 2.0392_r8k, &
      &            1.5861_r8k, 2.1636_r8k, 2.6365_r8k, 2.9826_r8k, 3.2548_r8k, 3.4781_r8k, &
      &            0.8878_r8k, 0.5426_r8k, 0.3511_r8k, 0.2979_r8k, 0.2755_r8k, 0.2642_r8k, &
      &            0.8887_r8k, 0.5431_r8k, 0.3514_r8k, 0.2982_r8k, 0.2757_r8k, 0.2644_r8k, &
      &            0.9022_r8k, 0.6540_r8k, 0.4239_r8k, 0.3474_r8k, 0.3142_r8k, 0.2966_r8k, &
      &            0.9207_r8k, 0.8171_r8k, 0.5876_r8k, 0.4574_r8k, 0.3962_r8k, 0.3629_r8k, &
      &            0.9340_r8k, 0.9198_r8k, 0.7728_r8k, 0.6047_r8k, 0.5034_r8k, 0.4465_r8k, &
      &            0.9450_r8k, 0.9878_r8k, 0.9387_r8k, 0.7941_r8k, 0.6597_r8k, 0.5686_r8k, &
      &            0.9587_r8k, 1.0563_r8k, 1.1089_r8k, 1.0541_r8k, 0.9406_r8k, 0.8226_r8k, &
      &            0.9734_r8k, 1.1151_r8k, 1.2519_r8k, 1.3109_r8k, 1.3105_r8k, 1.2600_r8k, &
      &            0.9921_r8k, 1.1757_r8k, 1.3767_r8k, 1.5150_r8k, 1.6133_r8k, 1.6787_r8k, &
      &            1.0115_r8k, 1.2267_r8k, 1.4660_r8k, 1.6438_r8k, 1.7871_r8k, 1.9055_r8k, &
      &            1.5859_r8k, 1.9618_r8k, 2.3710_r8k, 2.6764_r8k, 2.9239_r8k, 3.1300_r8k, &
      &            0.8887_r8k, 0.6464_r8k, 0.3685_r8k, 0.2912_r8k, 0.2607_r8k, 0.2458_r8k, &
      &            0.8896_r8k, 0.6471_r8k, 0.3689_r8k, 0.2915_r8k, 0.2609_r8k, 0.2461_r8k, &
      &            0.9028_r8k, 0.7509_r8k, 0.4718_r8k, 0.3554_r8k, 0.3076_r8k, 0.2835_r8k, &
      &            0.9211_r8k, 0.8724_r8k, 0.6946_r8k, 0.5155_r8k, 0.4193_r8k, 0.3689_r8k, &
      &            0.9341_r8k, 0.9393_r8k, 0.8865_r8k, 0.7321_r8k, 0.5863_r8k, 0.4923_r8k, &
      &            0.9450_r8k, 0.9832_r8k, 1.0135_r8k, 0.9429_r8k, 0.8147_r8k, 0.6881_r8k, &
      &            0.9584_r8k, 1.0284_r8k, 1.1308_r8k, 1.1521_r8k, 1.1131_r8k, 1.0258_r8k, &
      &            0.9729_r8k, 1.0664_r8k, 1.2211_r8k, 1.3137_r8k, 1.3687_r8k, 1.3900_r8k, &
      &            0.9913_r8k, 1.1070_r8k, 1.3014_r8k, 1.4392_r8k, 1.5503_r8k, 1.6403_r8k, &
      &            1.0106_r8k, 1.1425_r8k, 1.3602_r8k, 1.5182_r8k, 1.6497_r8k, 1.7622_r8k, &
      &            1.5856_r8k, 1.8164_r8k, 2.1827_r8k, 2.4482_r8k, 2.6686_r8k, 2.8568_r8k, &
      &            0.8905_r8k, 0.7988_r8k, 0.4516_r8k, 0.3070_r8k, 0.2548_r8k, 0.2310_r8k, &
      &            0.8914_r8k, 0.7996_r8k, 0.4521_r8k, 0.3073_r8k, 0.2551_r8k, 0.2312_r8k, &
      &            0.9042_r8k, 0.8540_r8k, 0.6125_r8k, 0.4137_r8k, 0.3240_r8k, 0.2820_r8k, &
      &            0.9218_r8k, 0.9121_r8k, 0.8539_r8k, 0.6848_r8k, 0.5213_r8k, 0.4215_r8k, &
      &            0.9344_r8k, 0.9435_r8k, 0.9773_r8k, 0.9305_r8k, 0.8080_r8k, 0.6635_r8k, &
      &            0.9449_r8k, 0.9644_r8k, 1.0457_r8k, 1.0720_r8k, 1.0408_r8k, 0.9567_r8k, &
      &            0.9578_r8k, 0.9866_r8k, 1.1068_r8k, 1.1871_r8k, 1.2290_r8k, 1.2379_r8k, &
      &            0.9718_r8k, 1.0055_r8k, 1.1479_r8k, 1.2577_r8k, 1.3407_r8k, 1.4084_r8k, &
      &            0.9897_r8k, 1.0274_r8k, 1.1873_r8k, 1.3146_r8k, 1.4161_r8k, 1.5061_r8k, &
      &            1.0087_r8k, 1.0491_r8k, 1.2201_r8k, 1.3553_r8k, 1.4628_r8k, 1.5580_r8k, &
      &            1.5848_r8k, 1.6590_r8k, 1.9449_r8k, 2.1700_r8k, 2.3475_r8k, 2.5037_r8k, &
      &            0.8924_r8k, 0.8715_r8k, 0.6053_r8k, 0.3613_r8k, 0.2708_r8k, 0.2318_r8k, &
      &            0.8933_r8k, 0.8724_r8k, 0.6059_r8k, 0.3617_r8k, 0.2710_r8k, 0.2320_r8k, &
      &            0.9056_r8k, 0.8986_r8k, 0.7662_r8k, 0.5308_r8k, 0.3816_r8k, 0.3064_r8k, &
      &            0.9226_r8k, 0.9266_r8k, 0.9215_r8k, 0.8527_r8k, 0.6988_r8k, 0.5449_r8k, &
      &            0.9347_r8k, 0.9421_r8k, 0.9865_r8k, 1.0158_r8k, 0.9814_r8k, 0.8932_r8k, &
      &            0.9448_r8k, 0.9531_r8k, 1.0200_r8k, 1.0931_r8k, 1.1223_r8k, 1.1166_r8k, &
      &            0.9573_r8k, 0.9656_r8k, 1.0504_r8k, 1.1531_r8k, 1.2213_r8k, 1.2704_r8k, &
      &            0.9707_r8k, 0.9772_r8k, 1.0704_r8k, 1.1863_r8k, 1.2696_r8k, 1.3391_r8k, &
      &            0.9881_r8k, 0.9920_r8k, 1.0921_r8k, 1.2171_r8k, 1.3079_r8k, 1.3848_r8k, &
      &            1.0067_r8k, 1.0085_r8k, 1.1131_r8k, 1.2432_r8k, 1.3364_r8k, 1.4144_r8k, &
      &            1.5838_r8k, 1.5923_r8k, 1.7687_r8k, 1.9850_r8k, 2.1390_r8k, 2.2666_r8k, &
      &            0.8943_r8k, 0.9025_r8k, 0.7664_r8k, 0.4660_r8k, 0.3114_r8k, 0.2470_r8k, &
      &            0.8952_r8k, 0.9034_r8k, 0.7672_r8k, 0.4665_r8k, 0.3117_r8k, 0.2472_r8k, &
      &            0.9071_r8k, 0.9167_r8k, 0.8628_r8k, 0.6870_r8k, 0.4826_r8k, 0.3629_r8k, &
      &            0.9234_r8k, 0.9316_r8k, 0.9411_r8k, 0.9391_r8k, 0.8664_r8k, 0.7290_r8k, &
      &            0.9350_r8k, 0.9408_r8k, 0.9701_r8k, 1.0306_r8k, 1.0541_r8k, 1.0340_r8k, &
      &            0.9447_r8k, 0.9479_r8k, 0.9849_r8k, 1.0693_r8k, 1.1292_r8k, 1.1620_r8k, &
      &            0.9567_r8k, 0.9567_r8k, 0.9993_r8k, 1.1006_r8k, 1.1807_r8k, 1.2396_r8k, &
      &            0.9696_r8k, 0.9654_r8k, 1.0091_r8k, 1.1171_r8k, 1.2040_r8k, 1.2698_r8k, &
      &            0.9864_r8k, 0.9774_r8k, 1.0213_r8k, 1.1348_r8k, 1.2257_r8k, 1.2942_r8k, &
      &            1.0048_r8k, 0.9919_r8k, 1.0357_r8k, 1.1525_r8k, 1.2448_r8k, 1.3130_r8k, &
      &            1.5827_r8k, 1.5658_r8k, 1.6421_r8k, 1.8366_r8k, 1.9894_r8k, 2.1013_r8k, &
      &            0.8983_r8k, 0.9191_r8k, 0.9184_r8k, 0.7311_r8k, 0.4574_r8k, 0.3176_r8k, &
      &            0.8992_r8k, 0.9201_r8k, 0.9193_r8k, 0.7319_r8k, 0.4579_r8k, 0.3180_r8k, &
      &            0.9101_r8k, 0.9265_r8k, 0.9377_r8k, 0.8860_r8k, 0.7333_r8k, 0.5401_r8k, &
      &            0.9251_r8k, 0.9349_r8k, 0.9474_r8k, 0.9752_r8k, 1.0023_r8k, 0.9752_r8k, &
      &            0.9357_r8k, 0.9407_r8k, 0.9500_r8k, 0.9951_r8k, 1.0636_r8k, 1.1068_r8k, &
      &            0.9446_r8k, 0.9456_r8k, 0.9517_r8k, 1.0020_r8k, 1.0831_r8k, 1.1437_r8k, &
      &            0.9556_r8k, 0.9521_r8k, 0.9543_r8k, 1.0083_r8k, 1.0981_r8k, 1.1676_r8k, &
      &            0.9675_r8k, 0.9589_r8k, 0.9564_r8k, 1.0103_r8k, 1.1020_r8k, 1.1725_r8k, &
      &            0.9831_r8k, 0.9687_r8k, 0.9608_r8k, 1.0142_r8k, 1.1074_r8k, 1.1783_r8k, &
      &            1.0008_r8k, 0.9818_r8k, 0.9695_r8k, 1.0221_r8k, 1.1158_r8k, 1.1858_r8k, &
      &            1.5800_r8k, 1.5516_r8k, 1.5344_r8k, 1.6237_r8k, 1.7791_r8k, 1.8945_r8k, &
      &            0.9045_r8k, 0.9269_r8k, 0.9562_r8k, 0.9698_r8k, 0.8087_r8k, 0.5428_r8k, &
      &            0.9054_r8k, 0.9278_r8k, 0.9571_r8k, 0.9707_r8k, 0.8095_r8k, 0.5433_r8k, &
      &            0.9148_r8k, 0.9322_r8k, 0.9541_r8k, 0.9754_r8k, 0.9661_r8k, 0.8693_r8k, &
      &            0.9277_r8k, 0.9379_r8k, 0.9495_r8k, 0.9637_r8k, 1.0006_r8k, 1.0507_r8k, &
      &            0.9368_r8k, 0.9418_r8k, 0.9464_r8k, 0.9540_r8k, 0.9963_r8k, 1.0662_r8k, &
      &            0.9445_r8k, 0.9454_r8k, 0.9449_r8k, 0.9475_r8k, 0.9901_r8k, 1.0651_r8k, &
      &            0.9540_r8k, 0.9502_r8k, 0.9441_r8k, 0.9414_r8k, 0.9834_r8k, 1.0622_r8k, &
      &            0.9643_r8k, 0.9553_r8k, 0.9436_r8k, 0.9355_r8k, 0.9748_r8k, 1.0523_r8k, &
      &            0.9780_r8k, 0.9632_r8k, 0.9451_r8k, 0.9311_r8k, 0.9676_r8k, 1.0442_r8k, &
      &            0.9946_r8k, 0.9750_r8k, 0.9516_r8k, 0.9327_r8k, 0.9668_r8k, 1.0424_r8k, &
      &            1.5753_r8k, 1.5443_r8k, 1.5074_r8k, 1.4783_r8k, 1.5362_r8k, 1.6616_r8k, &
      &            0.9265_r8k, 0.9499_r8k, 0.9810_r8k, 1.0244_r8k, 1.1015_r8k, 1.1778_r8k, &
      &            0.9275_r8k, 0.9509_r8k, 0.9820_r8k, 1.0254_r8k, 1.1026_r8k, 1.1790_r8k, &
      &            0.9315_r8k, 0.9494_r8k, 0.9720_r8k, 0.9990_r8k, 1.0343_r8k, 1.0844_r8k, &
      &            0.9369_r8k, 0.9471_r8k, 0.9586_r8k, 0.9693_r8k, 0.9758_r8k, 0.9838_r8k, &
      &            0.9407_r8k, 0.9456_r8k, 0.9498_r8k, 0.9515_r8k, 0.9458_r8k, 0.9383_r8k, &
      &            0.9440_r8k, 0.9447_r8k, 0.9437_r8k, 0.9394_r8k, 0.9266_r8k, 0.9113_r8k, &
      &            0.9482_r8k, 0.9442_r8k, 0.9377_r8k, 0.9271_r8k, 0.9069_r8k, 0.8842_r8k, &
      &            0.9531_r8k, 0.9441_r8k, 0.9319_r8k, 0.9157_r8k, 0.8893_r8k, 0.8604_r8k, &
      &            0.9607_r8k, 0.9460_r8k, 0.9277_r8k, 0.9053_r8k, 0.8727_r8k, 0.8380_r8k, &
      &            0.9736_r8k, 0.9541_r8k, 0.9307_r8k, 0.9032_r8k, 0.8655_r8k, 0.8264_r8k, &
      &            1.5575_r8k, 1.5240_r8k, 1.4849_r8k, 1.4397_r8k, 1.3789_r8k, 1.3166_r8k, &
      &            0.9579_r8k, 0.9775_r8k, 1.0037_r8k, 1.0407_r8k, 1.1082_r8k, 1.1920_r8k, &
      &            0.9589_r8k, 0.9785_r8k, 1.0047_r8k, 1.0417_r8k, 1.1093_r8k, 1.1932_r8k, &
      &            0.9526_r8k, 0.9670_r8k, 0.9852_r8k, 1.0070_r8k, 1.0357_r8k, 1.0776_r8k, &
      &            0.9444_r8k, 0.9520_r8k, 0.9603_r8k, 0.9676_r8k, 0.9707_r8k, 0.9717_r8k, &
      &            0.9391_r8k, 0.9421_r8k, 0.9443_r8k, 0.9441_r8k, 0.9369_r8k, 0.9243_r8k, &
      &            0.9355_r8k, 0.9351_r8k, 0.9331_r8k, 0.9281_r8k, 0.9152_r8k, 0.8961_r8k, &
      &            0.9314_r8k, 0.9274_r8k, 0.9211_r8k, 0.9113_r8k, 0.8928_r8k, 0.8680_r8k, &
      &            0.9291_r8k, 0.9214_r8k, 0.9110_r8k, 0.8971_r8k, 0.8743_r8k, 0.8451_r8k, &
      &            0.9301_r8k, 0.9185_r8k, 0.9040_r8k, 0.8860_r8k, 0.8591_r8k, 0.8263_r8k, &
      &            0.9445_r8k, 0.9299_r8k, 0.9122_r8k, 0.8911_r8k, 0.8613_r8k, 0.8260_r8k, &
      &            1.5765_r8k, 1.5506_r8k, 1.5204_r8k, 1.4852_r8k, 1.4364_r8k, 1.3796_r8k, &
      &            0.9042_r8k, 0.9075_r8k, 0.9125_r8k, 0.9200_r8k, 0.9349_r8k, 0.9555_r8k, &
      &            0.9051_r8k, 0.9084_r8k, 0.9134_r8k, 0.9209_r8k, 0.9359_r8k, 0.9565_r8k, &
      &            0.9050_r8k, 0.9072_r8k, 0.9103_r8k, 0.9145_r8k, 0.9205_r8k, 0.9302_r8k, &
      &            0.9057_r8k, 0.9066_r8k, 0.9077_r8k, 0.9087_r8k, 0.9089_r8k, 0.9087_r8k, &
      &            0.9073_r8k, 0.9074_r8k, 0.9073_r8k, 0.9067_r8k, 0.9046_r8k, 0.9011_r8k, &
      &            0.9102_r8k, 0.9097_r8k, 0.9088_r8k, 0.9072_r8k, 0.9039_r8k, 0.8989_r8k, &
      &            0.9145_r8k, 0.9134_r8k, 0.9117_r8k, 0.9093_r8k, 0.9048_r8k, 0.8984_r8k, &
      &            0.9231_r8k, 0.9215_r8k, 0.9192_r8k, 0.9161_r8k, 0.9108_r8k, 0.9036_r8k, &
      &            0.9419_r8k, 0.9400_r8k, 0.9373_r8k, 0.9338_r8k, 0.9281_r8k, 0.9203_r8k, &
      &            0.9876_r8k, 0.9860_r8k, 0.9837_r8k, 0.9804_r8k, 0.9748_r8k, 0.9672_r8k, &
      &            1.7953_r8k, 1.7923_r8k, 1.7882_r8k, 1.7825_r8k, 1.7728_r8k, 1.7596_r8k /), &
      &            (/ NGdConcVals,NRadVals,NBUVals /)) 
    !
    ! HWR Power Profile Values
    !
    REAL(r8k), DIMENSION(NGdConcVals,NRadVals,NBUVals), PARAMETER, PRIVATE :: gdPPHWR = &
      &   reshape( (/ 0.8874_r8k, 0.7311_r8k, 0.7397_r8k, 0.7472_r8k, 0.7535_r8k, 0.7588_r8k, &
      &            0.8883_r8k, 0.7319_r8k, 0.7404_r8k, 0.7480_r8k, 0.7543_r8k, 0.7596_r8k, &
      &            0.9021_r8k, 0.7546_r8k, 0.7610_r8k, 0.7674_r8k, 0.7729_r8k, 0.7776_r8k, &
      &            0.9213_r8k, 0.7905_r8k, 0.7933_r8k, 0.7979_r8k, 0.8021_r8k, 0.8058_r8k, &
      &            0.9350_r8k, 0.8210_r8k, 0.8205_r8k, 0.8235_r8k, 0.8267_r8k, 0.8296_r8k, &
      &            0.9460_r8k, 0.8503_r8k, 0.8465_r8k, 0.8480_r8k, 0.8502_r8k, 0.8523_r8k, &
      &            0.9595_r8k, 0.8894_r8k, 0.8810_r8k, 0.8804_r8k, 0.8812_r8k, 0.8823_r8k, &
      &            0.9740_r8k, 0.9489_r8k, 0.9331_r8k, 0.9290_r8k, 0.9276_r8k, 0.9271_r8k, &
      &            0.9928_r8k, 1.0495_r8k, 1.0235_r8k, 1.0125_r8k, 1.0068_r8k, 1.0032_r8k, &
      &            1.0115_r8k, 1.1992_r8k, 1.1714_r8k, 1.1514_r8k, 1.1381_r8k, 1.1287_r8k, &
      &            1.5822_r8k, 2.2337_r8k, 2.2897_r8k, 2.2948_r8k, 2.2868_r8k, 2.2749_r8k, &
      &            0.8852_r8k, 0.6368_r8k, 0.6393_r8k, 0.6484_r8k, 0.6576_r8k, 0.6662_r8k, &
      &            0.8861_r8k, 0.6374_r8k, 0.6399_r8k, 0.6491_r8k, 0.6583_r8k, 0.6669_r8k, &
      &            0.9001_r8k, 0.6707_r8k, 0.6657_r8k, 0.6721_r8k, 0.6798_r8k, 0.6874_r8k, &
      &            0.9198_r8k, 0.7275_r8k, 0.7083_r8k, 0.7096_r8k, 0.7144_r8k, 0.7202_r8k, &
      &            0.9338_r8k, 0.7808_r8k, 0.7472_r8k, 0.7431_r8k, 0.7451_r8k, 0.7491_r8k, &
      &            0.9451_r8k, 0.8352_r8k, 0.7874_r8k, 0.7772_r8k, 0.7761_r8k, 0.7781_r8k, &
      &            0.9590_r8k, 0.9109_r8k, 0.8464_r8k, 0.8263_r8k, 0.8201_r8k, 0.8189_r8k, &
      &            0.9739_r8k, 1.0194_r8k, 0.9453_r8k, 0.9087_r8k, 0.8925_r8k, 0.8852_r8k, &
      &            0.9934_r8k, 1.1693_r8k, 1.1245_r8k, 1.0740_r8k, 1.0402_r8k, 1.0190_r8k, &
      &            1.0133_r8k, 1.3280_r8k, 1.3737_r8k, 1.3576_r8k, 1.3267_r8k, 1.2956_r8k, &
      &            1.5902_r8k, 2.2842_r8k, 2.5222_r8k, 2.6338_r8k, 2.6893_r8k, 2.7135_r8k, &
      &            0.8851_r8k, 0.5800_r8k, 0.5780_r8k, 0.5830_r8k, 0.5897_r8k, 0.5962_r8k, &
      &            0.8860_r8k, 0.5806_r8k, 0.5786_r8k, 0.5836_r8k, 0.5902_r8k, 0.5968_r8k, &
      &            0.8999_r8k, 0.6093_r8k, 0.6103_r8k, 0.6104_r8k, 0.6145_r8k, 0.6195_r8k, &
      &            0.9193_r8k, 0.6586_r8k, 0.6657_r8k, 0.6557_r8k, 0.6549_r8k, 0.6569_r8k, &
      &            0.9332_r8k, 0.7064_r8k, 0.7208_r8k, 0.6989_r8k, 0.6926_r8k, 0.6914_r8k, &
      &            0.9443_r8k, 0.7589_r8k, 0.7818_r8k, 0.7456_r8k, 0.7324_r8k, 0.7274_r8k, &
      &            0.9581_r8k, 0.8417_r8k, 0.8765_r8k, 0.8187_r8k, 0.7931_r8k, 0.7810_r8k, &
      &            0.9730_r8k, 0.9863_r8k, 1.0274_r8k, 0.9515_r8k, 0.9036_r8k, 0.8764_r8k, &
      &            0.9926_r8k, 1.2184_r8k, 1.2356_r8k, 1.1926_r8k, 1.1370_r8k, 1.0900_r8k, &
      &            1.0131_r8k, 1.4637_r8k, 1.4319_r8k, 1.4819_r8k, 1.4905_r8k, 1.4756_r8k, &
      &            1.5954_r8k, 2.5960_r8k, 2.4933_r8k, 2.6781_r8k, 2.8014_r8k, 2.8889_r8k, &
      &            0.8851_r8k, 0.5902_r8k, 0.5404_r8k, 0.5398_r8k, 0.5442_r8k, 0.5492_r8k, &
      &            0.8860_r8k, 0.5908_r8k, 0.5410_r8k, 0.5403_r8k, 0.5447_r8k, 0.5497_r8k, &
      &            0.8997_r8k, 0.6580_r8k, 0.5803_r8k, 0.5714_r8k, 0.5721_r8k, 0.5749_r8k, &
      &            0.9188_r8k, 0.7739_r8k, 0.6547_r8k, 0.6270_r8k, 0.6195_r8k, 0.6177_r8k, &
      &            0.9326_r8k, 0.8681_r8k, 0.7346_r8k, 0.6839_r8k, 0.6662_r8k, 0.6589_r8k, &
      &            0.9436_r8k, 0.9409_r8k, 0.8239_r8k, 0.7499_r8k, 0.7185_r8k, 0.7040_r8k, &
      &            0.9573_r8k, 1.0180_r8k, 0.9505_r8k, 0.8593_r8k, 0.8050_r8k, 0.7762_r8k, &
      &            0.9721_r8k, 1.0905_r8k, 1.1058_r8k, 1.0416_r8k, 0.9705_r8k, 0.9173_r8k, &
      &            0.9917_r8k, 1.1675_r8k, 1.2661_r8k, 1.2822_r8k, 1.2526_r8k, 1.2039_r8k, &
      &            1.0128_r8k, 1.2391_r8k, 1.3990_r8k, 1.4864_r8k, 1.5363_r8k, 1.5623_r8k, &
      &            1.6003_r8k, 2.0630_r8k, 2.4037_r8k, 2.6182_r8k, 2.7703_r8k, 2.8859_r8k, &
      &            0.8851_r8k, 0.6218_r8k, 0.5181_r8k, 0.5088_r8k, 0.5104_r8k, 0.5140_r8k, &
      &            0.8860_r8k, 0.6224_r8k, 0.5186_r8k, 0.5093_r8k, 0.5109_r8k, 0.5145_r8k, &
      &            0.8995_r8k, 0.7056_r8k, 0.5687_r8k, 0.5457_r8k, 0.5418_r8k, 0.5423_r8k, &
      &            0.9184_r8k, 0.8275_r8k, 0.6702_r8k, 0.6150_r8k, 0.5978_r8k, 0.5913_r8k, &
      &            0.9320_r8k, 0.9068_r8k, 0.7806_r8k, 0.6921_r8k, 0.6566_r8k, 0.6410_r8k, &
      &            0.9429_r8k, 0.9609_r8k, 0.8885_r8k, 0.7856_r8k, 0.7274_r8k, 0.6986_r8k, &
      &            0.9564_r8k, 1.0170_r8k, 1.0131_r8k, 0.9308_r8k, 0.8508_r8k, 0.7984_r8k, &
      &            0.9712_r8k, 1.0679_r8k, 1.1330_r8k, 1.1176_r8k, 1.0593_r8k, 0.9937_r8k, &
      &            0.9909_r8k, 1.1249_r8k, 1.2487_r8k, 1.3036_r8k, 1.3173_r8k, 1.3014_r8k, &
      &            1.0125_r8k, 1.1815_r8k, 1.3497_r8k, 1.4527_r8k, 1.5215_r8k, 1.5684_r8k, &
      &            1.6050_r8k, 1.9638_r8k, 2.3108_r8k, 2.5388_r8k, 2.7061_r8k, 2.8364_r8k, &
      &            0.8853_r8k, 0.7305_r8k, 0.5102_r8k, 0.4730_r8k, 0.4671_r8k, 0.4677_r8k, &
      &            0.8862_r8k, 0.7312_r8k, 0.5107_r8k, 0.4735_r8k, 0.4676_r8k, 0.4681_r8k, &
      &            0.8993_r8k, 0.8022_r8k, 0.5908_r8k, 0.5234_r8k, 0.5064_r8k, 0.5016_r8k, &
      &            0.9177_r8k, 0.8820_r8k, 0.7501_r8k, 0.6316_r8k, 0.5844_r8k, 0.5654_r8k, &
      &            0.9310_r8k, 0.9271_r8k, 0.8819_r8k, 0.7630_r8k, 0.6790_r8k, 0.6378_r8k, &
      &            0.9416_r8k, 0.9571_r8k, 0.9704_r8k, 0.8966_r8k, 0.8004_r8k, 0.7323_r8k, &
      &            0.9548_r8k, 0.9892_r8k, 1.0543_r8k, 1.0426_r8k, 0.9802_r8k, 0.9019_r8k, &
      &            0.9694_r8k, 1.0182_r8k, 1.1229_r8k, 1.1661_r8k, 1.1710_r8k, 1.1430_r8k, &
      &            0.9891_r8k, 1.0535_r8k, 1.1924_r8k, 1.2754_r8k, 1.3296_r8k, 1.3613_r8k, &
      &            1.0118_r8k, 1.0938_r8k, 1.2606_r8k, 1.3696_r8k, 1.4521_r8k, 1.5165_r8k, &
      &            1.6137_r8k, 1.8151_r8k, 2.1558_r8k, 2.3852_r8k, 2.5622_r8k, 2.7043_r8k, &
      &            0.8856_r8k, 0.8194_r8k, 0.5522_r8k, 0.4633_r8k, 0.4429_r8k, 0.4382_r8k, &
      &            0.8865_r8k, 0.8202_r8k, 0.5527_r8k, 0.4637_r8k, 0.4433_r8k, 0.4387_r8k, &
      &            0.8992_r8k, 0.8592_r8k, 0.6679_r8k, 0.5370_r8k, 0.4941_r8k, 0.4797_r8k, &
      &            0.9171_r8k, 0.9031_r8k, 0.8394_r8k, 0.7043_r8k, 0.6099_r8k, 0.5664_r8k, &
      &            0.9300_r8k, 0.9283_r8k, 0.9343_r8k, 0.8677_r8k, 0.7621_r8k, 0.6802_r8k, &
      &            0.9404_r8k, 0.9455_r8k, 0.9903_r8k, 0.9782_r8k, 0.9151_r8k, 0.8292_r8k, &
      &            0.9533_r8k, 0.9647_r8k, 1.0430_r8k, 1.0768_r8k, 1.0691_r8k, 1.0281_r8k, &
      &            0.9677_r8k, 0.9831_r8k, 1.0836_r8k, 1.1495_r8k, 1.1862_r8k, 1.2013_r8k, &
      &            0.9874_r8k, 1.0081_r8k, 1.1293_r8k, 1.2183_r8k, 1.2826_r8k, 1.3327_r8k, &
      &            1.0110_r8k, 1.0409_r8k, 1.1825_r8k, 1.2893_r8k, 1.3700_r8k, 1.4370_r8k, &
      &            1.6218_r8k, 1.7275_r8k, 2.0248_r8k, 2.2518_r8k, 2.4246_r8k, 2.5685_r8k, &
      &            0.8860_r8k, 0.8651_r8k, 0.6456_r8k, 0.4797_r8k, 0.4347_r8k, 0.4208_r8k, &
      &            0.8869_r8k, 0.8660_r8k, 0.6462_r8k, 0.4801_r8k, 0.4351_r8k, 0.4212_r8k, &
      &            0.8993_r8k, 0.8877_r8k, 0.7632_r8k, 0.5884_r8k, 0.5052_r8k, 0.4731_r8k, &
      &            0.9166_r8k, 0.9125_r8k, 0.8871_r8k, 0.8001_r8k, 0.6782_r8k, 0.5975_r8k, &
      &            0.9291_r8k, 0.9274_r8k, 0.9461_r8k, 0.9335_r8k, 0.8656_r8k, 0.7710_r8k, &
      &            0.9393_r8k, 0.9383_r8k, 0.9791_r8k, 1.0074_r8k, 0.9898_r8k, 0.9386_r8k, &
      &            0.9518_r8k, 0.9512_r8k, 1.0108_r8k, 1.0710_r8k, 1.0940_r8k, 1.0920_r8k, &
      &            0.9660_r8k, 0.9649_r8k, 1.0358_r8k, 1.1146_r8k, 1.1632_r8k, 1.1967_r8k, &
      &            0.9857_r8k, 0.9852_r8k, 1.0681_r8k, 1.1623_r8k, 1.2267_r8k, 1.2791_r8k, &
      &            1.0102_r8k, 1.0150_r8k, 1.1123_r8k, 1.2220_r8k, 1.2987_r8k, 1.3624_r8k, &
      &            1.6292_r8k, 1.6867_r8k, 1.9057_r8k, 2.1408_r8k, 2.3088_r8k, 2.4476_r8k, &
      &            0.8871_r8k, 0.8950_r8k, 0.8196_r8k, 0.5831_r8k, 0.4602_r8k, 0.4177_r8k, &
      &            0.8880_r8k, 0.8959_r8k, 0.8204_r8k, 0.5836_r8k, 0.4606_r8k, 0.4181_r8k, &
      &            0.8996_r8k, 0.9051_r8k, 0.8745_r8k, 0.7407_r8k, 0.5864_r8k, 0.5039_r8k, &
      &            0.9158_r8k, 0.9172_r8k, 0.9181_r8k, 0.9071_r8k, 0.8391_r8k, 0.7287_r8k, &
      &            0.9276_r8k, 0.9256_r8k, 0.9358_r8k, 0.9701_r8k, 0.9726_r8k, 0.9347_r8k, &
      &            0.9371_r8k, 0.9326_r8k, 0.9460_r8k, 0.9988_r8k, 1.0315_r8k, 1.0372_r8k, &
      &            0.9491_r8k, 0.9415_r8k, 0.9573_r8k, 1.0243_r8k, 1.0758_r8k, 1.1075_r8k, &
      &            0.9627_r8k, 0.9523_r8k, 0.9687_r8k, 1.0429_r8k, 1.1029_r8k, 1.1451_r8k, &
      &            0.9822_r8k, 0.9697_r8k, 0.9879_r8k, 1.0710_r8k, 1.1390_r8k, 1.1882_r8k, &
      &            1.0082_r8k, 0.9982_r8k, 1.0221_r8k, 1.1177_r8k, 1.1963_r8k, 1.2536_r8k, &
      &            1.6426_r8k, 1.6668_r8k, 1.7494_r8k, 1.9608_r8k, 2.1357_r8k, 2.2652_r8k, &
      &            0.8893_r8k, 0.9021_r8k, 0.9161_r8k, 0.8258_r8k, 0.6078_r8k, 0.4814_r8k, &
      &            0.8902_r8k, 0.9030_r8k, 0.9170_r8k, 0.8267_r8k, 0.6084_r8k, 0.4819_r8k, &
      &            0.9005_r8k, 0.9093_r8k, 0.9202_r8k, 0.8996_r8k, 0.7945_r8k, 0.6518_r8k, &
      &            0.9151_r8k, 0.9181_r8k, 0.9226_r8k, 0.9341_r8k, 0.9485_r8k, 0.9225_r8k, &
      &            0.9256_r8k, 0.9246_r8k, 0.9242_r8k, 0.9412_r8k, 0.9831_r8k, 1.0088_r8k, &
      &            0.9343_r8k, 0.9302_r8k, 0.9262_r8k, 0.9442_r8k, 0.9951_r8k, 1.0342_r8k, &
      &            0.9452_r8k, 0.9377_r8k, 0.9297_r8k, 0.9485_r8k, 1.0064_r8k, 1.0532_r8k, &
      &            0.9580_r8k, 0.9472_r8k, 0.9357_r8k, 0.9538_r8k, 1.0151_r8k, 1.0649_r8k, &
      &            0.9771_r8k, 0.9636_r8k, 0.9493_r8k, 0.9682_r8k, 1.0347_r8k, 1.0891_r8k, &
      &            1.0051_r8k, 0.9926_r8k, 0.9793_r8k, 1.0031_r8k, 1.0793_r8k, 1.1418_r8k, &
      &            1.6597_r8k, 1.6717_r8k, 1.6796_r8k, 1.7548_r8k, 1.9271_r8k, 2.0703_r8k, &
      &            0.8991_r8k, 0.9146_r8k, 0.9374_r8k, 0.9838_r8k, 1.0418_r8k, 0.9630_r8k, &
      &            0.9000_r8k, 0.9155_r8k, 0.9384_r8k, 0.9848_r8k, 1.0429_r8k, 0.9640_r8k, &
      &            0.9059_r8k, 0.9170_r8k, 0.9322_r8k, 0.9569_r8k, 0.9967_r8k, 1.0315_r8k, &
      &            0.9143_r8k, 0.9195_r8k, 0.9256_r8k, 0.9312_r8k, 0.9393_r8k, 0.9721_r8k, &
      &            0.9207_r8k, 0.9217_r8k, 0.9220_r8k, 0.9187_r8k, 0.9134_r8k, 0.9341_r8k, &
      &            0.9263_r8k, 0.9241_r8k, 0.9204_r8k, 0.9116_r8k, 0.8993_r8k, 0.9142_r8k, &
      &            0.9335_r8k, 0.9278_r8k, 0.9198_r8k, 0.9056_r8k, 0.8869_r8k, 0.8975_r8k, &
      &            0.9433_r8k, 0.9341_r8k, 0.9222_r8k, 0.9032_r8k, 0.8795_r8k, 0.8868_r8k, &
      &            0.9605_r8k, 0.9480_r8k, 0.9325_r8k, 0.9093_r8k, 0.8807_r8k, 0.8857_r8k, &
      &            0.9933_r8k, 0.9800_r8k, 0.9636_r8k, 0.9386_r8k, 0.9075_r8k, 0.9134_r8k, &
      &            1.7030_r8k, 1.6977_r8k, 1.6860_r8k, 1.6562_r8k, 1.6122_r8k, 1.6378_r8k, &
      &            0.9128_r8k, 0.9280_r8k, 0.9495_r8k, 0.9911_r8k, 1.0538_r8k, 1.1613_r8k, &
      &            0.9137_r8k, 0.9289_r8k, 0.9505_r8k, 0.9921_r8k, 1.0549_r8k, 1.1625_r8k, &
      &            0.9111_r8k, 0.9221_r8k, 0.9364_r8k, 0.9584_r8k, 0.9934_r8k, 1.0370_r8k, &
      &            0.9085_r8k, 0.9139_r8k, 0.9200_r8k, 0.9250_r8k, 0.9296_r8k, 0.9322_r8k, &
      &            0.9077_r8k, 0.9095_r8k, 0.9105_r8k, 0.9077_r8k, 0.9003_r8k, 0.8849_r8k, &
      &            0.9082_r8k, 0.9073_r8k, 0.9048_r8k, 0.8975_r8k, 0.8840_r8k, 0.8605_r8k, &
      &            0.9098_r8k, 0.9059_r8k, 0.8998_r8k, 0.8880_r8k, 0.8691_r8k, 0.8389_r8k, &
      &            0.9158_r8k, 0.9090_r8k, 0.8997_r8k, 0.8841_r8k, 0.8612_r8k, 0.8263_r8k, &
      &            0.9323_r8k, 0.9227_r8k, 0.9104_r8k, 0.8914_r8k, 0.8648_r8k, 0.8257_r8k, &
      &            0.9779_r8k, 0.9668_r8k, 0.9530_r8k, 0.9323_r8k, 0.9037_r8k, 0.8613_r8k, &
      &            1.8021_r8k, 1.7859_r8k, 1.7653_r8k, 1.7325_r8k, 1.6853_r8k, 1.6094_r8k, &
      &            0.8674_r8k, 0.8735_r8k, 0.8829_r8k, 0.9014_r8k, 0.9313_r8k, 0.9847_r8k, &
      &            0.8682_r8k, 0.8744_r8k, 0.8838_r8k, 0.9023_r8k, 0.9322_r8k, 0.9857_r8k, &
      &            0.8695_r8k, 0.8738_r8k, 0.8799_r8k, 0.8897_r8k, 0.9061_r8k, 0.9279_r8k, &
      &            0.8731_r8k, 0.8750_r8k, 0.8774_r8k, 0.8797_r8k, 0.8820_r8k, 0.8830_r8k, &
      &            0.8775_r8k, 0.8778_r8k, 0.8780_r8k, 0.8768_r8k, 0.8735_r8k, 0.8658_r8k, &
      &            0.8828_r8k, 0.8821_r8k, 0.8808_r8k, 0.8776_r8k, 0.8714_r8k, 0.8596_r8k, &
      &            0.8908_r8k, 0.8889_r8k, 0.8861_r8k, 0.8808_r8k, 0.8720_r8k, 0.8569_r8k, &
      &            0.9060_r8k, 0.9031_r8k, 0.8990_r8k, 0.8921_r8k, 0.8814_r8k, 0.8640_r8k, &
      &            0.9386_r8k, 0.9350_r8k, 0.9300_r8k, 0.9219_r8k, 0.9096_r8k, 0.8904_r8k, &
      &            1.0147_r8k, 1.0114_r8k, 1.0066_r8k, 0.9983_r8k, 0.9856_r8k, 0.9655_r8k, &
      &            2.0113_r8k, 2.0049_r8k, 1.9953_r8k, 1.9792_r8k, 1.9549_r8k, 1.9164_r8k /), &
      &            (/ NGdConcVals,NRadVals,NBUVals /))
    !
    CONTAINS
    !
    SUBROUTINE LoadGadProperties ()
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This subroutine loads the Gad radial burnup power profiles
    !>@author
    !> Ian Porter, NRC
    !>@date
    !> 11/13/2015
    !
    INTEGER(ipk) :: RxType
    !
    ALLOCATE (GadPow(1:2))
    ! LWR
    RxType = 1
    GadPow(RxType)%NRadVals = SIZE(Gdrad)
    GadPow(RxType)%NBUVals = SIZE(GdBu)
    GadPow(RxType)%NGdConcVals = SIZE(GdConc)
    ALLOCATE(GadPow(RxType)%Radius(1:GadPow(RxType)%NRadVals))
    ALLOCATE(GadPow(RxType)%Burnup(1:GadPow(RxType)%NBUVals))
    ALLOCATE(GadPow(RxType)%GadConc(1:GadPow(RxType)%NGdConcVals))
    ALLOCATE(GadPow(RxType)%Power(1:GadPow(RxType)%NGdConcVals, 1:GadPow(RxType)%NRadVals, 1:GadPow(RxType)%NBUVals))
    GadPow(RxType)%Radius = Gdrad
    GadPow(RxType)%Burnup = GdBu
    GadPow(RxType)%GadConc = GdConc
    GadPow(RxType)%Power = gdPPLWR
    ! HWR
    RxType = 2
    GadPow(RxType)%NRadVals = SIZE(Gdrad)
    GadPow(RxType)%NBUVals = SIZE(GdBu)
    GadPow(RxType)%NGdConcVals = SIZE(GdConc)
    ALLOCATE(GadPow(RxType)%Radius(1:GadPow(RxType)%NRadVals))
    ALLOCATE(GadPow(RxType)%Burnup(1:GadPow(RxType)%NBUVals))
    ALLOCATE(GadPow(RxType)%GadConc(1:GadPow(RxType)%NGdConcVals))
    ALLOCATE(GadPow(RxType)%Power(1:GadPow(RxType)%NGdConcVals, 1:GadPow(RxType)%NRadVals, 1:GadPow(RxType)%NBUVals))
    GadPow(RxType)%Radius = Gdrad
    GadPow(RxType)%Burnup = GdBu
    GadPow(RxType)%GadConc = GdConc
    GadPow(RxType)%Power = gdppHWR
    !
    END SUBROUTINE LoadGadProperties
    !
    !
    !
    SUBROUTINE Gdradpow (rprm1, rpow, gadolin, rnorm, buave)
    USE Kinds
    USE conversions_frapcon
    IMPLICIT NONE
    !>@brief
    !> This Subroutine calculates the radial power profile for urania-gadolinia fuel as a function
    !> of gadolinia concentration, normalized fuel pellet radius, and rod average burnup for LWR conditions
    !>@author
    !> Updated by Ian Porter, NRC
    !>@date
    !> 1/14/2015
    !
    ! Input
    !
    ! gadolin  - weight percent gadolinia
    ! rnorm    - normalized fuel pellet radius, r/r(outer)
    ! buave    - rod average burnup for axial node
    !
    ! Output
    !
    ! rpow     - relative radial power at radius, rnorm
    !
    ! Internal
    !
    ! gd(i,j,k) - array stores gd radial power profile based on:
    !         i - gad concentration (0%, 2%, 4%, 6%, 8%, 10%, array GdConc) If > 10%, uses profile for 10%.
    !         j - normalized radial dimension (0-1, array Gdrad)
    !         k - burnup values (0 - 60 GWd/MTU, array GdBu). If > 60, uses profile for 60.
    !
    INTEGER(ipk) :: RxType
    REAL(r8k) :: pow1, pow2, powerlow, powerhigh
    REAL(r8k), INTENT(IN) :: rprm1, gadolin, rnorm, buave
    REAL(r8k), INTENT(OUT) :: rpow
    REAL(r8k), DIMENSION(:), ALLOCATABLE :: dumarray1, dumarray2
    INTEGER(ipk) :: i, j, k
    ! Data tables for UO2/Gd2O3 radial power profiles
    IF (rprm1 > 3.0_r8k) THEN
        ! LWR
        RxType = 1
    ELSE
        ! HWR
        RxType = 2
    END IF
    ! Allocate dummy arrays
    ALLOCATE (dumarray1(1:GadPow(RxType)%NRadVals), dumarray2(1:GadPow(RxType)%NRadVals))
    ! Find range of Gd2O3 concentration
    DO k = 1, (GadPow(RxType)%NGdConcVals - 1)
        IF (gadolin <= GadPow(RxType)%GadConc(k+1)) EXIT
    END DO
    ! Find relative power for low bound concentration
    DO i = 2, GadPow(RxType)%NBUVals
        IF (buave <= GadPow(RxType)%Burnup(i)) EXIT
    END DO
    ! Ensure array does not exceed maximum bounds
    IF ((i == (GadPow(RxType)%NBUVals + 1) .OR. k == GadPow(RxType)%NGdConcVals) .AND. (.NOT. WarnMsg)) THEN
        IF (i == (GadPow(RxType)%NBUVals + 1)) THEN
            WRITE (0,100) 
            WRITE (ounit,100)
100         FORMAT('Bounds of gd array have been exceeded in Gdradpow. Burnup > 60 GWd/MTU')
        END IF
        IF (k == GadPow(RxType)%NGdConcVals) THEN
            WRITE (0,101)
            WRITE (ounit,101)
101         FORMAT('Bounds of gd array have been exceeded in Gdradpow. Gd Concentration > 10.0%')
        END IF
        WarnMsg = .TRUE.
    END IF
    DO j = 1, GadPow(RxType)%NRadVals
        dumarray1(j) = GadPow(RxType)%Power(k,j,MIN(i-1,GadPow(RxType)%NBUVals))
        dumarray2(j) = GadPow(RxType)%Power(k,j,MIN(i,GadPow(RxType)%NBUVals))
    END DO
    pow1 = terp(rnorm, GadPow(RxType)%Radius, dumarray1, GadPow(RxType)%NRadVals)
    pow2 = terp(rnorm, GadPow(RxType)%Radius, dumarray2, GadPow(RxType)%NRadVals)
    IF (i == (GadPow(RxType)%NBUVals + 1)) THEN
        powerlow = pow1
    ELSE
        powerlow = (buave - GadPow(RxType)%Burnup(i-1)) / (GadPow(RxType)%Burnup(i) - GadPow(RxType)%Burnup(i-1)) * &
          &        (pow2 - pow1) + pow1
    END IF
    ! Find relative power for upper bound
    DO i = 2, GadPow(RxType)%NBUVals
        IF (buave <= GadPow(RxType)%Burnup(i)) EXIT
    END DO
    DO j = 1, GadPow(RxType)%NRadVals
        dumarray1(j) = GadPow(RxType)%Power(MIN(k+1,GadPow(RxType)%NGdConcVals),j,MIN(i-1,GadPow(RxType)%NBUVals))
        dumarray2(j) = GadPow(RxType)%Power(MIN(k+1,GadPow(RxType)%NGdConcVals),j,MIN(i,GadPow(RxType)%NBUVals))
    END DO
    pow1 = terp(rnorm, GadPow(RxType)%Radius, dumarray1, GadPow(RxType)%NRadVals)
    pow2 = terp(rnorm, GadPow(RxType)%Radius, dumarray2, GadPow(RxType)%NRadVals)
    IF (i == (GadPow(RxType)%NBUVals + 1)) THEN
        powerhigh = pow1
    ELSE
        powerhigh = (buave - GadPow(RxType)%Burnup(i-1)) / (GadPow(RxType)%Burnup(i) - GadPow(RxType)%Burnup(i-1)) * &
          &         (pow2 - pow1) + pow1
    END IF
    ! Interporlate between lower and upper bounds
    IF (k == GadPow(RxType)%NGdConcVals) THEN
        rpow = powerlow
    ELSE
        rpow = (gadolin - GadPow(RxType)%GadConc(k)) / (GadPow(RxType)%GadConc(k+1) - GadPow(RxType)%GadConc(k)) * &
          &    (powerhigh - powerlow) + powerlow
    END IF
    ! Deallocate dummy arrays
    DEALLOCATE (dumarray1, dumarray2)
    !
    END SUBROUTINE GdRadPow
    !
END MODULE GadRadPower



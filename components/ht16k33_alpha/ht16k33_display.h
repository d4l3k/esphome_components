#pragma once

#include "esphome/core/component.h"
#include "esphome/core/defines.h"
#include "esphome/components/i2c/i2c.h"

#ifdef USE_TIME
#include "esphome/components/time/real_time_clock.h"
#include "esphome/core/time.h"
#endif

namespace esphome {
namespace ht16k33_alpha {

#define HT16K33 HT16K33AlphaDisplay
//#define displayRAM this->buffer_
#define numberOfDisplays this->displays_.size()


#define DEFAULT_ADDRESS 0x70 // Default I2C address when A0, A1 are floating
#define DEFAULT_NOTHING_ATTACHED 0xFF

// Define constants for segment bits
#define SEG_A 0x0001
#define SEG_B 0x0002
#define SEG_C 0x0004
#define SEG_D 0x0008
#define SEG_E 0x0010
#define SEG_F 0x0020
#define SEG_G 0x0040
#define SEG_H 0x0080
#define SEG_I 0x0100
#define SEG_J 0x0200
#define SEG_K 0x0400
#define SEG_L 0x0800
#define SEG_M 0x1000
#define SEG_N 0x2000


typedef enum
{
    ALPHA_BLINK_RATE_NOBLINK = 0b00,
    ALPHA_BLINK_RATE_2HZ = 0b01,
    ALPHA_BLINK_RATE_1HZ = 0b10,
    ALPHA_BLINK_RATE_0_5HZ = 0b11,
} alpha_blink_rate_t;

typedef enum
{
    ALPHA_DISPLAY_ON = 0b1,
    ALPHA_DISPLAY_OFF = 0b0,
} alpha_display_t;

typedef enum
{
    ALPHA_DECIMAL_ON = 0b1,
    ALPHA_DECIMAL_OFF = 0b0,
} alpha_decimal_t;

typedef enum
{
    ALPHA_COLON_ON = 0b1,
    ALPHA_COLON_OFF = 0b0,
} alpha_colon_t;

typedef enum
{
    ALPHA_CMD_SYSTEM_SETUP = 0b00100000,
    ALPHA_CMD_DISPLAY_SETUP = 0b10000000,
    ALPHA_CMD_DIMMING_SETUP = 0b11100000,
} alpha_command_t;

// Structure for defining new character displays
struct CharDef {
  uint8_t  position;
  int16_t segments;
  struct CharDef * next;
};

class HT16K33AlphaDisplay : public PollingComponent, public i2c::I2CDevice {
 public:
  void set_writer(std::function<void(HT16K33AlphaDisplay &)> &&writer) { this->writer_ = std::move(writer); }
  void setup() override;
  void loop() override;
  float get_setup_priority() const override;
  void add_secondary_display(i2c::I2CDevice *display) { this->displays_.push_back(display); }
  void set_scroll(bool scroll) { this->scroll_ = scroll; }
  void set_continuous(bool continuous) { this->continuous_ = continuous; }
  void set_scroll_speed(unsigned long scroll_speed) { this->scroll_speed_ = scroll_speed; }
  void set_scroll_dwell(unsigned long scroll_dwell) { this->scroll_dwell_ = scroll_dwell; }
  void set_scroll_delay(unsigned long scroll_delay) { this->scroll_delay_ = scroll_delay; }
  void update() override;
  //// Clear display
  void set_brightness(float level);
  float get_brightness();

  /// Print the given text
  void print(const char *str);
  /// Print the given string
  void print(const std::string &str);
  /// Evaluate the printf-format and print the text
  void printf(const char *format, ...) __attribute__((format(printf, 2, 3)));

  // For overloading the print function
  virtual size_t write(uint8_t);
  virtual size_t write(const uint8_t *buffer, size_t size);
  virtual size_t write(const char *str);

#ifdef USE_TIME
  /// Evaluate the strftime-format and print the text
  void strftime(const char *format, ESPTime time) __attribute__((format(strftime, 2, 0)));
#endif

 protected:
  void command_(uint8_t value);
  void call_writer() { this->writer_(*this); }
  void display_();

  std::vector<i2c::I2CDevice *> displays_ {this};
  std::function<void(HT16K33AlphaDisplay &)> writer_;
  bool scroll_ {false};
  bool continuous_ {false};
  unsigned long scroll_speed_ {250};
  unsigned long scroll_dwell_ {2000};
  unsigned long scroll_delay_ {750};
  unsigned long last_scroll_ {0};
  //std::vector<uint8_t> buffer_{16*4};
  int offset_ {0};
  uint8_t brightness_ = 16;
  
  protected:
    uint8_t lookUpDisplayAddress(uint8_t displayNumber);

    //Display configuration functions
    bool clear();
    bool displayOn();
    bool displayOff();
    bool displayOnSingle(uint8_t displayNumber);
    bool displayOffSingle(uint8_t displayNumber);
    bool setDisplayOnOff(uint8_t displayNumber, bool turnOnDisplay);

    bool enableSystemClock();
    bool disableSystemClock();
    bool enableSystemClockSingle(uint8_t displayNumber);
    bool disableSystemClockSingle(uint8_t displayNumber);

    // Light up functions
    void illuminateSegment(uint8_t segment, uint8_t digit);
    void illuminateChar(uint16_t disp, uint8_t digit);
    void printChar(uint8_t displayChar, uint8_t digit);
    bool updateDisplay();

    // Define Character Segment Map
    bool defineChar(uint8_t displayChar, uint16_t segmentsToTurnOn);
    uint16_t getSegmentsToTurnOn (uint8_t charPos);

    // Decimal functions
    bool decimalOn();
    bool decimalOff();
    bool decimalOnSingle(uint8_t displayNumber, bool updateNow = true);
    bool decimalOffSingle(uint8_t displayNumber, bool updateNow = true);
    bool setDecimalOnOff(uint8_t displayNumber, bool turnOnDecimal, bool updateNow = true);
    
    // Colon functions
    bool colonOn();
    bool colonOff();
    bool colonOnSingle(uint8_t displayNumber, bool updateNow = true);
    bool colonOffSingle(uint8_t displayNumber, bool updateNow = true);
    bool setColonOnOff(uint8_t displayNumber, bool turnOnColon, bool updateNow = true);

    // Shifting
    bool shiftRight(uint8_t shiftAmt = 1);
    bool shiftLeft(uint8_t shiftAmt = 1);

    // I2C abstraction
    bool readRAM(uint8_t address, uint8_t reg, uint8_t *buff, uint8_t buffSize);
    bool writeRAM(uint8_t address, uint8_t reg, uint8_t *buff, uint8_t buffSize);
    bool writeRAM(uint8_t reg, uint8_t data);

private:
    //uint8_t _deviceAddressDisplayOne; // Address of primary alphanumeric display
    //uint8_t _deviceAddressDisplayTwo;
    //uint8_t _deviceAddressDisplayThree;
    //uint8_t _deviceAddressDisplayFour;
    uint8_t digitPosition = 0;
    //uint8_t numberOfDisplays = 1;
    bool displayOnOff = 0; // Tracks display on/off bit of display setup register
    bool decimalOnOff = 0;
    bool colonOnOff = 0;
    uint8_t blinkRate = ALPHA_BLINK_RATE_NOBLINK; // Tracks blink bits in display setup register

    // Enough RAM for up to 4 displays on same I2C bus
    uint8_t displayRAM[16 * 4];
    char displayContent[4 * 4 + 1] = "";

    // Linked List of character definitions
    struct CharDef * pCharDefList = NULL;
};

}  // namespace ht16k33_alpha
}  // namespace esphome

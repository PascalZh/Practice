/**
 *  @file pca9685Test.c
 *  
 *  @author     Jonas Scharpf <jonas@brainelectronics.de>
 *  @date       Januar 2018
 *  @version    1.0
 *  
 *  @brief      set PWM of outputs of PCA9685 slave chip
 *  
 *  Description:
 *      I2C Slave device PCA9685 at adress 0x40 can be controlled
 *      to set individual PWM to the outputs
 *      The PWM frequency can only be set for all outputs to same value
 *      
 *  Circuit:
 *      PCA9685 attached to pins 4, 5
 *      I2C Connection:
 *          Board   SDA       SCL
 *          ESP32   any (5)   any (4)
 *          Mega    20        21
 *          Tiny    D0/pin 5  D2/pin 7  (Digispark, Digispark Pro)
 *          Uno     A4        A5
 *  
 */
#include <driver/i2c.h>
#include <esp_log.h>
#include <freertos/FreeRTOS.h>
#include <freertos/task.h>
#include <math.h>

#include "pca9685.h"

#include "sdkconfig.h"

#define I2C_EXAMPLE_MASTER_SCL_IO   4    /*!< gpio number for I2C master clock */
#define I2C_EXAMPLE_MASTER_SDA_IO   5    /*!< gpio number for I2C master data  */
#define I2C_EXAMPLE_MASTER_FREQ_HZ  100000     /*!< I2C master clock frequency */
#define I2C_EXAMPLE_MASTER_NUM      I2C_NUM_0   /*!< I2C port number for master dev */
#define I2C_EXAMPLE_MASTER_TX_BUF_DISABLE   0   /*!< I2C master do not need buffer */
#define I2C_EXAMPLE_MASTER_RX_BUF_DISABLE   0   /*!< I2C master do not need buffer */


#define I2C_ADDRESS     0x40    /*!< lave address for PCA9685 */

#define ACK_CHECK_EN    0x1     /*!< I2C master will check ack from slave */
#define ACK_CHECK_DIS   0x0     /*!< I2C master will not check ack from slave */
#define ACK_VAL         0x0     /*!< I2C ack value */
#define NACK_VAL        0x1     /*!< I2C nack value */

static char tag[] = "PCA9685";

#undef ESP_ERROR_CHECK
#define ESP_ERROR_CHECK(x)   do { esp_err_t rc = (x); if (rc != ESP_OK) { ESP_LOGE("err", "esp_err_t = %d", rc); assert(0 && #x);} } while(0);

static void i2c_example_master_init(void);

/**
 * @brief i2c master initialization
 */
static void i2c_example_master_init(void)
{
	ESP_LOGD(tag, ">> PCA9685");
	i2c_config_t conf;
	conf.mode = I2C_MODE_MASTER;
	conf.sda_io_num = I2C_EXAMPLE_MASTER_SDA_IO;
	conf.scl_io_num = I2C_EXAMPLE_MASTER_SCL_IO;
	conf.sda_pullup_en = GPIO_PULLUP_ENABLE;
	conf.scl_pullup_en = GPIO_PULLUP_ENABLE;
	conf.master.clk_speed = I2C_EXAMPLE_MASTER_FREQ_HZ;

	int i2c_master_port = I2C_EXAMPLE_MASTER_NUM;
	ESP_ERROR_CHECK(i2c_param_config(i2c_master_port, &conf));
	ESP_ERROR_CHECK(i2c_driver_install(i2c_master_port, conf.mode,
				I2C_EXAMPLE_MASTER_RX_BUF_DISABLE,
				I2C_EXAMPLE_MASTER_TX_BUF_DISABLE, 0));
}

void p_setAngle(uint8_t port, int angle);//0度到180度
void p_sleep(float second);
void app_main()
{
	printf("Executing on core %d\n", xPortGetCoreID());
	i2c_example_master_init();

	set_pca9685_adress(I2C_ADDRESS);
	resetPCA9685();

	setFrequencyPCA9685(50);  // 1000 Hz
	vTaskDelay(1000 / portTICK_RATE_MS);

	printf("Finished setup, entering loop now\n");

#define SERVOMIN_SG90 100
#define SERVOMAX_SG90 460

	while(1)
	{
		/*printf("Freq: 50Hz\n");*/

		/*printf("MAX%% for 3 seconds:\n");*/
		/*setPWM(15, 0, SERVOMAX_SG90);*/
		/*vTaskDelay(3000 / portTICK_RATE_MS);*/

		/*printf("MED%% for 3 seconds:\n");*/
		/*setPWM(15, 0, (SERVOMAX_SG90+SERVOMIN_SG90)/2);*/
		/*vTaskDelay(3000 / portTICK_RATE_MS);*/

		/*printf("MIN%% for 3 seconds:\n");*/
		/*setPWM(15, 0, SERVOMIN_SG90);*/
		/*vTaskDelay(3000 / portTICK_RATE_MS);*/

		p_setAngle(15, 0);
		p_sleep(1);

		for (int i = 0; i <= 180; i++) {
			p_setAngle(15, i);
			p_sleep(0.01);
		}
		for (int i = 180; i >= 0; i--) {
			p_setAngle(15, i);
			p_sleep(0.01);
		}
		p_sleep(2);
		p_setAngle(15, 180);
		p_sleep(0.4);
		p_setAngle(15, 0);
		p_sleep(1);
		p_setAngle(15, 45);
		p_sleep(1);
		p_setAngle(15, 90);
		p_sleep(1);
		p_setAngle(15, 135);
		p_sleep(1);
		p_setAngle(15, 180);

		p_sleep(1);
	}
}
void p_setAngle(uint8_t port, int angle)//0度到180度
{
	if (angle > 180 || angle < 0) return;
	setPWM(port, 0, 2*angle + 100);
}
void p_sleep(float second)
{
	vTaskDelay(second*1000 / portTICK_RATE_MS);
}

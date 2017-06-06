{application,i2c,
             [{description,"I2C device controller"},
              {vsn,"0.0.0+build.42.ref67af275"},
              {mod,{i2c_app,[]}},
              {registered,[i2c_srv,i2c_port]},
              {env,[]},
              {applications,[kernel,stdlib]},
              {modules,[i2c,i2c_app,i2c_m24256,i2c_pca9685,i2c_si4703,
                        i2c_sup]}]}.

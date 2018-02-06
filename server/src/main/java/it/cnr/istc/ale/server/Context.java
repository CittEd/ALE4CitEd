/*
 * Copyright (C) 2018 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.ale.server;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.LessonState;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.NewParameter;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Context {

    private static final Logger LOG = Logger.getLogger(Context.class.getName());
    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ALE_PU");
    private static final ScheduledExecutorService EXECUTOR = Executors.newScheduledThreadPool(Runtime.getRuntime().availableProcessors());
    public static final String SERVER_ID = "server";
    private static Context context;
    public static final ObjectMapper MAPPER = new ObjectMapper();

    public static Context getContext() {
        if (context == null) {
            context = new Context();
        }
        return context;
    }
    MqttClient mqtt;
    /**
     * For each user id, a map of parameter types containing the name of the
     * parameter as key.
     */
    final Map<Long, Map<String, Parameter>> parameter_types = new HashMap<>();
    /**
     * For each user id, a map of parameter values containing the name of the
     * parameter as key. Notice that parameter values are represented through a
     * map.
     */
    final Map<Long, Map<String, Map<String, String>>> parameter_values = new HashMap<>();
    /**
     * For each lesson, the context of the lesson.
     */
    final Map<Long, LessonContext> lessons = new HashMap<>();
    final Lock lessons_lock = new ReentrantLock();

    private Context() {
        try {
            mqtt = new MqttClient("tcp://" + Config.getInstance().getParam(Config.Param.Host) + ":" + Config.getInstance().getParam(Config.Param.MQTTPort), SERVER_ID, new MemoryPersistence());
            mqtt.setCallback(new MqttCallback() {
                @Override
                public void connectionLost(Throwable cause) {
                    LOG.log(Level.SEVERE, null, cause);
                }

                @Override
                public void messageArrived(String topic, MqttMessage message) throws Exception {
                }

                @Override
                public void deliveryComplete(IMqttDeliveryToken token) {
                }
            });
            MqttConnectOptions options = new MqttConnectOptions();
            options.setCleanSession(false);
            options.setAutomaticReconnect(true);
            mqtt.connect(options);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }

        EXECUTOR.scheduleAtFixedRate(() -> {
            LOG.info("tick");
            lessons_lock.lock();
            try {
                try {
                    lessons.values().stream().filter(l -> l.getState() == LessonState.Running).forEach(l -> l.getManager().tick());
                } catch (Exception ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            } finally {
                lessons_lock.unlock();
            }
        }, 0, 5, TimeUnit.SECONDS);
    }

    public void addConnection(long user_id) {
        if (!parameter_types.containsKey(user_id)) {
            parameter_types.put(user_id, new HashMap<>());
        }
        if (!parameter_values.containsKey(user_id)) {
            parameter_values.put(user_id, new HashMap<>());
        }
        try {
            mqtt.publish(user_id + "/output/on-line", Boolean.TRUE.toString().getBytes(), 1, true);
            mqtt.subscribe(user_id + "/output", (String topic, MqttMessage message) -> {
                Message m = MAPPER.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    // the user has declared a new parameter..
                    NewParameter np = (NewParameter) m;
                    Parameter par = np.getParameter();
                    parameter_types.get(user_id).put(par.getName(), par);
                    mqtt.subscribe(user_id + "/output/" + par.getName(), (String par_topic, MqttMessage par_value) -> {
                        // the user has updated one of his/her parameters..
                        parameter_values.get(user_id).put(par.getName(), MAPPER.readValue(par_value.getPayload(), new TypeReference<Map<String, String>>() {
                        }));
                    });
                } else if (m instanceof LostParameter) {
                    // the user has removed a parameter..
                    LostParameter lp = (LostParameter) m;
                    mqtt.unsubscribe(user_id + "/output/" + lp.getName());
                    parameter_types.get(user_id).remove(lp.getName());
                    parameter_values.get(user_id).remove(lp.getName());
                } else {
                    LOG.log(Level.WARNING, "Not supported yet: {0}", m);
                }
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void removeConnection(long user_id) {
        try {
            for (Map.Entry<String, Parameter> par_type : parameter_types.get(user_id).entrySet()) {
                mqtt.unsubscribe(user_id + "/output/" + par_type.getKey());
            }
            mqtt.unsubscribe(user_id + "/output");
            mqtt.publish(user_id + "/output/on-line", Boolean.FALSE.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }
}

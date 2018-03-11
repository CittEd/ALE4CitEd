/*
 * Copyright (C) 2018 Riccardo De Benedictis
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
package it.cnr.istc.lecture.webapp;

import it.cnr.istc.lecture.api.Lesson;
import it.cnr.istc.lecture.api.Parameter;
import it.cnr.istc.lecture.api.messages.NewLesson;
import it.cnr.istc.lecture.api.messages.NewStudent;
import it.cnr.istc.lecture.api.model.LessonModel;
import it.cnr.istc.lecture.webapp.entities.UserEntity;
import it.cnr.istc.lecture.webapp.solver.LessonManager;
import it.cnr.istc.lecture.webapp.solver.LessonManagerListener;
import it.cnr.istc.lecture.webapp.solver.SolverToken;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.ConcurrencyManagement;
import javax.ejb.ConcurrencyManagementType;
import javax.ejb.Lock;
import javax.ejb.LockType;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.ws.rs.core.UriBuilder;
import org.apache.activemq.broker.Broker;
import org.apache.activemq.broker.BrokerFilter;
import org.apache.activemq.broker.BrokerPlugin;
import org.apache.activemq.broker.BrokerService;
import org.apache.activemq.broker.ConnectionContext;
import org.apache.activemq.broker.TransportConnector;
import org.apache.activemq.command.ConnectionInfo;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

/**
 *
 * @author Riccardo De Benedictis
 */
@Singleton
@Startup
@ConcurrencyManagement(ConcurrencyManagementType.CONTAINER)
public class LECTurEBean {

    private static final Logger LOG = Logger.getLogger(LECTurEBean.class.getName());
    public static final Jsonb JSONB = JsonbBuilder.create();
    private final Properties properties = new Properties();
    private BrokerService broker;
    private MqttClient mqtt;
    /**
     * For each user id, a map of parameter types containing the name of the
     * parameter as key.
     */
    private final Map<Long, Map<String, Parameter>> parameter_types = new HashMap<>();
    /**
     * For each user id, a map of parameter values containing the name of the
     * parameter as key. Notice that parameter values are represented through a
     * map.
     */
    private final Map<Long, Map<String, Map<String, String>>> parameter_values = new HashMap<>();
    /**
     * For each lesson, the context of the lesson.
     */
    private final Map<Long, LessonManager> lessons = new HashMap<>();
    @PersistenceContext
    private EntityManager em;

    @PostConstruct
    private void startup() {
        LOG.info("Starting LECTurE Server");

        try {
            properties.load(Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties"));
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
        final String mqtt_host = properties.getProperty("mqtt-host", "localhost");
        final String mqtt_port = properties.getProperty("mqtt-port", "1883");
        final String mqtt_server_id = properties.getProperty("mqtt-server-id", "LECTurE-Server");

        // we init the current state..
        for (UserEntity ue : em.createQuery("SELECT u FROM UserEntity u", UserEntity.class).getResultList()) {
            newUser(ue.getId());
        }

        // we start the MQTT broker..
        broker = new BrokerService();
        broker.setPersistent(false);
        try {
            TransportConnector connector = broker.addConnector(UriBuilder.fromUri("mqtt://" + mqtt_host + ":" + mqtt_port).build());
            broker.setPlugins(new BrokerPlugin[]{new BrokerPlugin() {
                @Override
                public Broker installPlugin(Broker broker) throws Exception {
                    return new BrokerFilter(broker) {
                        @Override
                        public void addConnection(ConnectionContext context, ConnectionInfo info) throws Exception {
                            LOG.log(Level.INFO, "New connection: {0}", info.getClientId());
                            if (!info.getClientId().equals(mqtt_server_id)) {
                                long user_id = Long.parseLong(info.getClientId());
                                mqtt.publish(user_id + "/output/on-line", Boolean.TRUE.toString().getBytes(), 1, true);
                                mqtt.subscribe(user_id + "/output", (String topic, MqttMessage message) -> {
                                    LOG.log(Level.INFO, "Message arrived: {0} {1}", new Object[]{topic, message});
                                });
                            }
                            super.addConnection(context, info);
                        }

                        @Override
                        public void removeConnection(ConnectionContext context, ConnectionInfo info, Throwable error) throws Exception {
                            LOG.log(Level.INFO, "Lost connection: {0}", info.getClientId());
                            if (!info.getClientId().equals(mqtt_server_id)) {
                                long user_id = Long.parseLong(info.getClientId());
                                for (Map.Entry<String, Parameter> par_type : parameter_types.get(user_id).entrySet()) {
                                    mqtt.unsubscribe(user_id + "/output/" + par_type.getKey());
                                }
                                mqtt.unsubscribe(user_id + "/output");
                                mqtt.publish(user_id + "/output/on-line", Boolean.FALSE.toString().getBytes(), 1, true);
                            }
                            super.removeConnection(context, info, error);
                        }
                    };
                }
            }});
            LOG.info("Starting MQTT Broker");
            broker.start();

            // we connect an MQTT client..
            mqtt = new MqttClient("tcp://" + mqtt_host + ":" + mqtt_port, mqtt_server_id, new MemoryPersistence());
            mqtt.setCallback(new MqttCallback() {
                @Override
                public void connectionLost(Throwable cause) {
                    LOG.log(Level.SEVERE, null, cause);
                }

                @Override
                public void messageArrived(String topic, MqttMessage message) throws Exception {
                    LOG.log(Level.WARNING, "Message arrived: {0} {1}", new Object[]{topic, message});
                }

                @Override
                public void deliveryComplete(IMqttDeliveryToken token) {
                }
            });
            MqttConnectOptions options = new MqttConnectOptions();
            options.setCleanSession(false);
            options.setAutomaticReconnect(true);
            LOG.info("Connecting MQTT Client");
            mqtt.connect(options);
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @PreDestroy
    private void shutdown() {
        LOG.info("Stopping LECTurE MQTT Broker");
        try {
            LOG.info("Disconnecting MQTT Client");
            mqtt.disconnect();
            mqtt.close();
            LOG.info("Stopping  MQTT Broker");
            broker.stop();
            parameter_types.clear();
            parameter_values.clear();
        } catch (Exception ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Lock(LockType.READ)
    public Map<String, Parameter> getParTypes(long user_id) {
        return Collections.unmodifiableMap(parameter_types.get(user_id));
    }

    @Lock(LockType.READ)
    public Map<String, Map<String, String>> getParValues(long user_id) {
        return Collections.unmodifiableMap(parameter_values.get(user_id));
    }

    @Lock(LockType.WRITE)
    public void newUser(long user_id) {
        parameter_types.put(user_id, new HashMap<>());
        parameter_values.put(user_id, new HashMap<>());
    }

    @Lock(LockType.WRITE)
    public void deleteUser(long user_id) {
        parameter_types.remove(user_id);
        parameter_values.remove(user_id);
    }

    @Lock(LockType.WRITE)
    public void newParameter(long user_id, Parameter p) {
        parameter_types.get(user_id).put(p.name, p);
    }

    @Lock(LockType.WRITE)
    public void newParameterValue(long user_id, String par, Map<String, String> val) {
        parameter_values.get(user_id).put(par, val);
    }

    @Lock(LockType.WRITE)
    public void newLesson(Lesson lesson, LessonModel model) {
        LessonManager manager = new LessonManager(lesson, model);
        lessons.put(lesson.id, manager);
        manager.addSolverListener(new LessonManagerListener() {
            @Override
            public void newToken(SolverToken tk) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void movedToken(SolverToken tk) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void executeToken(SolverToken tk) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void hideToken(SolverToken tk) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void removeToken(SolverToken tk) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void newTime(long time) {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }
        });

        // we notify all the students that a new lesson has been created..
        for (Long student_id : lesson.roles.values()) {
            try {
                mqtt.publish(student_id + "/input", JSONB.toJson(new NewLesson(lesson)).getBytes(), 1, false);
            } catch (MqttException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }

    @Lock(LockType.READ)
    public LessonManager getLessonManager(long lesson_id) {
        return lessons.get(lesson_id);
    }

    public void addTeacher(long student_id, long teacher_id) {
        try {
            mqtt.publish(teacher_id + "/input", JSONB.toJson(new NewStudent(student_id)).getBytes(), 1, false);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }
}

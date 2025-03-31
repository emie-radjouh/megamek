/*
 * MegaMek - Copyright (C) 2000-2005 Ben Mazur (bmazur@sev.org)
 * Copyright (c) 2024 - The MegaMek Team. All Rights Reserved.
 *
 * This file is part of MegaMek.
 *
 * MegaMek is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * MegaMek is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MegaMek. If not, see <http://www.gnu.org/licenses/>.
 */
package megamek.client.bot;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.util.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;

import megamek.client.AbstractClient;
import megamek.client.Client;
import megamek.client.bot.princess.CardinalEdge;
import megamek.client.ui.swing.ClientGUI;
import megamek.common.*;
import megamek.common.actions.EntityAction;
import megamek.common.actions.WeaponAttackAction;
import megamek.common.annotations.Nullable;
import megamek.common.enums.GamePhase;
import megamek.common.equipment.WeaponMounted;
import megamek.common.event.GameCFREvent;
import megamek.common.event.GameListenerAdapter;
import megamek.common.event.GamePhaseChangeEvent;
import megamek.common.event.GamePlayerChatEvent;
import megamek.common.event.GameReportEvent;
import megamek.common.event.GameTurnChangeEvent;
import megamek.common.net.packets.Packet;
import megamek.common.options.OptionsConstants;
import megamek.common.pathfinder.BoardClusterTracker;
import megamek.common.preference.PreferenceManager;
import megamek.common.util.BoardUtilities;
import megamek.common.util.StringUtil;
import megamek.logging.MMLogger;

public abstract class BotClient extends Client {
    private final static MMLogger logger = MMLogger.create(BotClient.class);

    public static final int BOT_TURN_RETRY_COUNT = 3;

    private List<Entity> currentTurnEnemyEntities;
    private List<Entity> currentTurnFriendlyEntities;

    // a frame, to show stuff in
    public JFrame frame;

    /**
     * Keeps track of whether this client has started to calculate a turn this
     * phase.
     */
    boolean calculatedTurnThisPhase = false;
    int calculatedTurnsThisPhase = 0;

    /**
     * Store a reference to the ClientGUI for the client who created this bot.
     * This is used to ensure keep the ClientGUI synchronized with changes to
     * this BotClient (particularly the bot's name).
     */
    private ClientGUI clientGUI = null;

    public class CalculateBotTurn implements Runnable {
        @Override
        public void run() {
            calculateMyTurn();
            flushConn();
        }
    }

    public BotClient(String playerName, String host, int port) {
        super(playerName, host, port);
        boardClusterTracker = new BoardClusterTracker();
        initializeListeners();
    }

    private void initializeListeners() {
        game.addGameListener(new GameListenerAdapter() {
            @Override
            public void gamePlayerChat(GamePlayerChatEvent e) {
                processChat(e);
                flushConn();
            }

            @Override
            public void gameTurnChange(GameTurnChangeEvent e) {
                handleGameTurnChange(e);
            }

            @Override
            public void gamePhaseChange(GamePhaseChangeEvent e) {
                handleGamePhaseChange(e);
            }

            @Override
            public void gameReport(GameReportEvent e) {
                if (game.getPhase().isInitiativeReport()) {
                    sendDone(true);
                    flushConn();
                }
            }
        });
    }

    private void handleGameTurnChange(GameTurnChangeEvent e) {
        boolean ignoreSimTurn = getGame().getPhase().isSimultaneous(getGame()) &&
              (e.getPreviousPlayerId() != localPlayerNumber) &&
              calculatedTurnThisPhase;
        if (isMyTurn() && !ignoreSimTurn) {
            calculatedTurnThisPhase = true;
            new Thread(new CalculateBotTurn(), getName() + " Turn " + game.getTurnIndex() + " Calc Thread").start();
            calculatedTurnsThisPhase++;
        }
        if (canUnloadStranded()) {
            sendUnloadStranded(getStrandedEntities());
        }
    }

    private void handleGamePhaseChange(GamePhaseChangeEvent e) {
        calculatedTurnThisPhase = false;
        if (e.getOldPhase().isSimultaneous(getGame())) {
            logger.info(String.format("%s: Calculated %d / %d turns for phase %s", getName(),
                  calculatedTurnsThisPhase, getGame().getEntitiesOwnedBy(getLocalPlayer()),
                  e.getOldPhase()));
        }
        calculatedTurnsThisPhase = 0;
    }

    @Override
    public boolean isBot() {
        return true;
    }

    BotConfiguration config = new BotConfiguration();

    protected BoardClusterTracker boardClusterTracker;

    public abstract void initialize();

    protected abstract void processChat(GamePlayerChatEvent ge);

    protected abstract void initMovement();

    protected abstract void initFiring();

    /**
     * Determines which entity should be moved next and then calls to
     * {@link #continueMovementFor(Entity)}
     * with that entity.
     *
     * @return The calculated move path.
     * @throws NullPointerException if no entity can be found to move.
     */
    protected abstract MovePath calculateMoveTurn();

    protected abstract void calculateFiringTurn();

    protected abstract void calculateDeployment() throws Exception;

    protected void initTargeting() {
    }

    /**
     * Calculates the targeting/off board turn
     * This includes firing TAG and non-direct-fire artillery
     * Does nothing in this implementation.
     */
    protected void calculateTargetingOffBoardTurn() {
        sendAttackData(game.getFirstEntityNum(getMyTurn()),
              new Vector<>(0));
        sendDone(true);
    }

    /**
     * Calculates the pre phase turn
     * currently does nothing other than end turn
     */
    protected void calculatePrePhaseTurn() {
        sendPrePhaseData(game.getFirstEntityNum(getMyTurn()));
        sendDone(true);
    }

    @Nullable
    protected abstract PhysicalOption calculatePhysicalTurn();

    protected Vector<EntityAction> calculatePointBlankShot(int firingEntityID, int targetID) {
        return new Vector<>();
    }

    protected int pickTagTarget(GameCFREvent evt) {
        return 0;
    }

    /**
     * Calculates the full {@link MovePath} for the given {@link Entity}.
     *
     * @param entity The entity who is to move.
     * @return The calculated move path.
     * @throws NullPointerException if entity is NULL.
     */
    protected abstract MovePath continueMovementFor(Entity entity);

    protected abstract Vector<Minefield> calculateMinefieldDeployment();

    protected abstract Vector<Coords> calculateArtyAutoHitHexes();

    protected abstract void checkMorale();

    @Override
    protected boolean keepGameLog() {
        return false;
    }

    /**
     * Helper function that determines which of this bot's entities are stranded
     * inside immobilized transports.
     *
     * @return Array of entity IDs.
     */

    public int[] getStrandedEntities() {
        return game.getPlayerEntities(getLocalPlayer(), true).stream()
              .filter(entity -> isStranded(entity))
              .mapToInt(Entity::getId)
              .toArray();
    }

    private boolean isStranded(Entity entity) {
        Entity transport = getTransportEntity(entity);
        return transport != null && transport.isPermanentlyImmobilized(true) &&
              !hasStackingViolation(entity, transport) && !isUnloadFatal(entity, transport);
    }

    private Entity getTransportEntity(Entity entity) {
        return entity.getTransportId() != Entity.NONE ? game.getEntity(entity.getTransportId()) : null;
    }

    private boolean hasStackingViolation(Entity entity, Entity transport) {
        return Compute.stackingViolation(game, entity.getId(), transport.getPosition(), entity.climbMode()) != null;
    }

    private boolean isUnloadFatal(Entity entity, Entity transport) {
        return entity.isBoardProhibited(game.getBoard().getType()) ||
              entity.isLocationProhibited(transport.getPosition()) ||
              entity.isLocationDeadly(transport.getPosition());
    }

    public List<Entity> getEntitiesOwned() {
        ArrayList<Entity> result = new ArrayList<>();
        for (Entity entity : game.getEntitiesVector()) {
            if (entity.getOwner().equals(getLocalPlayer())
                  && (entity.getPosition() != null) && !entity.isOffBoard()) {
                result.add(entity);
            }
        }
        return result;
    }

    protected Entity getArbitraryEntity() {
        for (Entity entity : game.getEntitiesVector()) {
            if (entity.getOwner().equals(getLocalPlayer())) {
                return entity;
            }
        }

        return null;
    }

    /**
     * Lazy-loaded list of enemy entities that we should consider firing at.
     * Only good for the current entity turn calculation, as this list can change
     * between individual entity turns.
     */
    public List<Entity> getEnemyEntities() {
        if (currentTurnEnemyEntities == null) {
            currentTurnEnemyEntities = new ArrayList<>();
            for (Entity entity : game.getEntitiesVector()) {
                if (entity.getOwner().isEnemyOf(getLocalPlayer())
                      && (entity.getPosition() != null) && !entity.isOffBoard()
                      && (entity.getCrew() != null) && !entity.getCrew().isDead()
                      && !entity.isHidden()) {
                    currentTurnEnemyEntities.add(entity);
                }
            }
        }

        return currentTurnEnemyEntities;
    }

    /**
     * Lazy-loaded list of friendly entities.
     * Only good for the current entity turn calculation, as this list can change
     * between individual entity turns.
     */
    public List<Entity> getFriendEntities() {
        if (currentTurnFriendlyEntities == null) {
            currentTurnFriendlyEntities = new ArrayList<>();
            for (Entity entity : game.getEntitiesVector()) {
                if (!entity.getOwner().isEnemyOf(getLocalPlayer()) && (entity.getPosition() != null)
                      && !entity.isOffBoard()) {
                    currentTurnFriendlyEntities.add(entity);
                }
            }
        }

        return currentTurnFriendlyEntities;
    }

    // TODO: move initMovement to be called on phase end
    @Override
    public void changePhase(GamePhase phase) {
        super.changePhase(phase);
        try {
            switch (phase) {
                case LOUNGE -> sendChat(Messages.getString("BotClient.Hi"));
                case DEPLOY_MINEFIELDS -> deployMinefields();
                case DEPLOYMENT -> initialize();
                case MOVEMENT -> handleMovementPhaseStart();
                case FIRING -> handleFiringPhaseStart();
                case TARGETING -> initTargeting();
                case END_REPORT, TARGETING_REPORT, INITIATIVE_REPORT, MOVEMENT_REPORT,
                      OFFBOARD_REPORT, FIRING_REPORT, PHYSICAL_REPORT -> sendDone(true);
                case VICTORY -> handleVictoryPhase();
                default -> {}
            }
        } catch (Throwable t) {
            logger.error(t, "changePhase");
        }
    }

    private void handleMovementPhaseStart() {
        if (!game.getOptions().booleanOption(OptionsConstants.ADVANCED_DOUBLE_BLIND) &&
              (game.getEntitiesOwnedBy(getLocalPlayer()) - game.getNoOfEntities()) == 0) {
            die();
        }
        if (Compute.randomInt(4) == 1) {
            String message = getRandomBotMessage();
            if (message != null) sendChat(message);
        }
        initMovement();
    }

    private void handleFiringPhaseStart() {
        postMovementProcessing();
        initFiring();
    }

    private void handleVictoryPhase() {
        runEndGame();
        sendChat(Messages.getString("BotClient.Bye"));
        die();
    }

    protected abstract void postMovementProcessing();

    private void runEndGame() {
        // Make a list of the player's living units.
        ArrayList<Entity> living = game.getPlayerEntities(getLocalPlayer(), false);

        // Be sure to include all units that have retreated.
        for (Enumeration<Entity> iter = game.getRetreatedEntities(); iter.hasMoreElements();) {
            Entity ent = iter.nextElement();
            if (ent.getOwnerId() == getLocalPlayer().getId()) {
                living.add(ent);
            }
        }

        if (living.isEmpty()) {
            return;
        }

        String sLogDir = PreferenceManager.getClientPreferences().getLogDirectory();
        File logDir = new File(sLogDir);
        if (!logDir.exists()) {
            // noinspection ResultOfMethodCallIgnored
            logDir.mkdir();
        }
        String fileName = "Bot_" + getLocalPlayer().getName() + ".mul";
        if (PreferenceManager.getClientPreferences().stampFilenames()) {
            fileName = StringUtil.addDateTimeStamp(fileName);
        }
        File unitFile = new File(sLogDir + File.separator + fileName);
        try {
            // Save the entities to the file.
            EntityListFile.saveTo(unitFile, living);
        } catch (Exception ex) {
            logger.error(ex, "runEndGame");
            doAlertDialog(Messages.getString("ClientGUI.errorSavingFile"), ex.getMessage());
        }
    }

    private Entity getRandomUnmovedEntity() {
        List<Entity> owned = getEntitiesOwned();
        List<Entity> unMoved = new ArrayList<>();
        for (Entity e : owned) {
            if (e.isSelectableThisTurn()) {
                unMoved.add(e);
            }
        }
        return unMoved.get(Compute.randomInt(unMoved.size()));
    }

    /**
     * Calculate what to do on my turn.
     * Has a retry mechanism for when the turn calculation fails due to concurrency
     * issues
     */
    private synchronized void calculateMyTurn() {
        int retryCount = 0;
        boolean success = false;

        while ((retryCount < BOT_TURN_RETRY_COUNT) && !success) {
            success = calculateMyTurnWorker();

            if (!success) {
                // if we fail, take a nap for 500-1500 milliseconds, then try again
                // as it may be due to some kind of thread-related issue
                // limit number of retries, so we're not endlessly spinning
                // if we can't recover from the error
                retryCount++;
                try {
                    Thread.sleep(Compute.randomInt(1000) + 500);
                } catch (InterruptedException e) {
                    logger.error(e, "calculateMyTune");
                }
            }
        }
    }

    /**
     * Worker function for a single attempt to calculate the bot's turn.
     */
    private synchronized boolean calculateMyTurnWorker() {
        currentTurnEnemyEntities = null;
        currentTurnFriendlyEntities = null;

        try {
            if (game.getPhase().isMovement()) {
                handleMovementPhase();
            } else if (game.getPhase().isFiring()) {
                calculateFiringTurn();
            } else if (game.getPhase().isPhysical()) {
                handlePhysicalPhase();
            } else if (game.getPhase().isDeployment()) {
                calculateDeployment();
            } else if (game.getPhase().isDeployMinefields()) {
                handleMinefieldDeployment();
            } else if (game.getPhase().isSetArtilleryAutohitHexes()) {
                handleArtilleryPhase();
            } else if (game.getPhase().isTargeting() || game.getPhase().isOffboard()) {
                calculateTargetingOffBoardTurn();
            } else if (game.getPhase().isPremovement() || game.getPhase().isPrefiring()) {
                calculatePrePhaseTurn();
            }
            return true;
        } catch (Exception ex) {
            logger.error(ex, "calculateMyTurnWorker");
            return false;
        }
    }

    private void handleMovementPhase() {
        MovePath mp = (game.getTurn() instanceof SpecificEntityTurn) ?
              continueMovementFor(game.getEntity(((SpecificEntityTurn) game.getTurn()).getEntityNum())) :
              (config.isForcedIndividual() ? continueMovementFor(getRandomUnmovedEntity()) : calculateMoveTurn());
        moveEntity(mp.getEntity().getId(), mp);
    }

    private void handlePhysicalPhase() {
        PhysicalOption po = calculatePhysicalTurn();
        sendAttackData((po != null) ? po.attacker.getId() : game.getFirstEntityNum(getMyTurn()),
              (po != null) ? po.getVector() : new Vector<>(0));
    }

    private void handleMinefieldDeployment() {
        Vector<Minefield> mines = calculateMinefieldDeployment();
        mines.forEach(game::addMinefield);
        sendDeployMinefields(mines);
        sendPlayerInfo();
    }

    private void handleArtilleryPhase() {
        sendArtyAutoHitHexes(calculateArtyAutoHitHexes());
    }

    public double getMassOfAllInBuilding(final Game game, final Coords coords) {
        double mass = 0;

        // Add the mass of anyone else standing in/on this building.
        final Hex hex = game.getBoard().getHex(coords);
        final int buildingElevation = hex.terrainLevel(Terrains.BLDG_ELEV);
        final int bridgeElevation = hex.terrainLevel(Terrains.BRIDGE_ELEV);
        Iterator<Entity> crowd = game.getEntities(coords);
        while (crowd.hasNext()) {
            Entity e = crowd.next();

            if (buildingElevation >= e.getElevation() || bridgeElevation >= e.getElevation()) {
                mass += e.getWeight();
            }
        }

        return mass;
    }

    /**
     * Gets valid and empty starting coords around the specified point. This
     * method iterates through the list of Coords and returns the first Coords
     * that does not have a stacking violation.
     */
    protected @Nullable Coords getFirstValidCoords(Entity deployedUnit,
          List<Coords> possibleDeployCoords) {
        // Check all of the hexes in order.
        for (Coords dest : possibleDeployCoords) {
            Entity violation = Compute.stackingViolation(game, deployedUnit,
                  dest, deployedUnit.getElevation(), dest, null, deployedUnit.climbMode());
            // Ignore coords that could cause a stacking violation
            if (violation != null) {
                continue;
            }

            // Make sure we don't overload any buildings in this hex.
            Building building = game.getBoard().getBuildingAt(dest);
            if (null != building) {
                double mass = getMassOfAllInBuilding(game, dest) + deployedUnit.getWeight();
                if (mass > building.getCurrentCF(dest)) {
                    continue;
                }
            }

            return dest;
        }

        // If NONE of them are acceptable, then just return null.
        return null;
    }

    protected List<Coords> getStartingCoordsArray(Entity deployedEnt) {
        List<RankedCoords> validCoords = getValidDeploymentCoords(deployedEnt);
        if (validCoords.isEmpty()) return Collections.emptyList();

        int highestElev = getMaxElevation(validCoords);
        int lowestElev = getMinElevation(validCoords);
        double avgRange = calculateAverageRange(deployedEnt);
        double idealElev = determineIdealElevation(deployedEnt, highestElev, lowestElev, avgRange);

        evaluateFitness(validCoords, deployedEnt, idealElev);
        Collections.sort(validCoords);

        return extractCoordinates(validCoords);
    }

    private List<RankedCoords> getValidDeploymentCoords(Entity deployedEnt) {
        List<RankedCoords> validCoords = new LinkedList<>();
        Board board = game.getBoard();

        for (int x = 0; x <= board.getWidth(); x++) {
            for (int y = 0; y <= board.getHeight(); y++) {
                Coords c = new Coords(x, y);
                if (isValidDeploymentLocation(board, c, deployedEnt)) {
                    validCoords.add(new RankedCoords(c, 0));
                }
            }
        }
        Collections.shuffle(validCoords);
        return validCoords;
    }

    private boolean isValidDeploymentLocation(Board board, Coords c, Entity deployedEnt) {
        return board.isLegalDeployment(c, deployedEnt) &&
              !deployedEnt.isLocationProhibited(c, deployedEnt.getElevation()) &&
              !deployedEnt.isLocationDeadly(c);
    }

    private int getMaxElevation(List<RankedCoords> validCoords) {
        return validCoords.stream().mapToInt(c -> game.getBoard().getHex(c.getX(), c.getY()).getLevel()).max().orElse(0);
    }

    private int getMinElevation(List<RankedCoords> validCoords) {
        return validCoords.stream().mapToInt(c -> game.getBoard().getHex(c.getX(), c.getY()).getLevel()).min().orElse(0);
    }

    private double calculateAverageRange(Entity deployedEnt) {
        return deployedEnt.getWeaponList().stream()
              .mapToDouble(w -> ((WeaponType) w.getType()).getLongRange())
              .average().orElse(0);
    }

    private double determineIdealElevation(Entity deployedEnt, int highestElev, int lowestElev, double avgRange) {
        if (deployedEnt.getJumpMP() == 0 && deployedEnt.getWalkMP() > 5) {
            return lowestElev + ((highestElev - lowestElev) / 3.0);
        }
        return Math.min(highestElev, lowestElev + ((avgRange / 18) * (highestElev - lowestElev)));
    }

    private void evaluateFitness(List<RankedCoords> validCoords, Entity deployedEnt, double idealElev) {
        for (RankedCoords coord : validCoords) {
            coord.setFitness(-Math.abs(idealElev - game.getBoard().getHex(coord.getX(), coord.getY()).getLevel()));
        }
    }

    private List<Coords> extractCoordinates(List<RankedCoords> validCoords) {
        return validCoords.stream().map(RankedCoords::getCoords).toList();
    }

    /**
     * Determines if the given entity has reasonable access to the "opposite" edge
     * of the board from its
     * current position. Returns 0 if this can be accomplished without destroying
     * any terrain,
     * -50 if this can be accomplished but terrain must be destroyed,
     * -100 if this cannot be accomplished at all
     */
    private int calculateEdgeAccessFitness(Entity entity, Board board) {
        // Flying units can always get anywhere
        if (entity.isAirborne() || entity instanceof VTOL) {
            return 0;
        }

        CardinalEdge destinationEdge = BoardUtilities.determineOppositeEdge(entity);

        int noReductionZoneSize = getClusterTracker().getDestinationCoords(entity, destinationEdge, false).size();
        int reductionZoneSize = getClusterTracker().getDestinationCoords(entity, destinationEdge, true).size();

        if (noReductionZoneSize > 0) {
            return 0;
        } else if (reductionZoneSize > 0) {
            return -50;
        } else {
            return -100;
        }
    }

    private double potentialBuildingDamage(int x, int y, Entity entity) {
        Coords coords = new Coords(x, y);
        Building building = game.getBoard().getBuildingAt(coords);
        if (building == null) {
            return 0;
        }
        int potentialDmg = (int) Math.ceil((double) building.getCurrentCF(coords) / 10);
        boolean aptGunnery = entity.hasAbility(OptionsConstants.PILOT_APTITUDE_GUNNERY);
        double oddsTakeDmg = 1 - (Compute.oddsAbove(entity.getCrew().getPiloting(), aptGunnery) / 100);
        return potentialDmg * oddsTakeDmg;
    }

    // Missile hits table
    // Some of these are interpolated for odd weapons sizes found in Protos and
    // new BAs
    private static float[] expectedHitsByRackSize = { 0.0f, 1.0f, 1.58f, 2.0f,
          2.63f, 3.17f, 4.0f, 4.49f, 4.98f, 5.47f, 6.31f, 7.23f, 8.14f,
          8.59f, 9.04f, 9.5f, 0.0f, 0.0f, 0.0f, 0.0f, 12.7f };

    /**
     * Determines the expected damage of a weapon attack, based on to-hit, salvo
     * sizes, etc. This has been copied almost wholesale from
     * Compute.getExpectedDamage; the log file print commands were removed due to
     * excessive data generated
     */
    private static float getDeployDamage(Game g, WeaponAttackAction waa, List<ECMInfo> allECMInfo) {
        Entity attacker = g.getEntity(waa.getEntityId());
        boolean naturalAptGunnery = attacker.hasAbility(OptionsConstants.PILOT_APTITUDE_GUNNERY);
        Mounted<?> weapon = attacker.getEquipment(waa.getWeaponId());
        ToHitData hitData = waa.toHit(g, allECMInfo);

        if (hitData.getValue() > 12) return 0.0f;
        float fChance = (hitData.getValue() == TargetRoll.AUTOMATIC_SUCCESS) ? 1.0f :
              (float) Compute.oddsAbove(hitData.getValue(), naturalAptGunnery) / 100.0f;

        return calculateExpectedDamage(weapon, waa, fChance);
    }

    private static float calculateExpectedDamage(Mounted<?> weapon, WeaponAttackAction waa, float fChance) {
        WeaponType wt = (WeaponType) weapon.getType();
        if (wt.getDamage() == WeaponType.DAMAGE_BY_CLUSTERTABLE) {
            return calculateClusterDamage(weapon, waa, fChance);
        }
        return wt.getDamage() * fChance;
    }

    private static float calculateClusterDamage(Mounted<?> weapon, WeaponAttackAction waa, float fChance) {
        WeaponType wt = (WeaponType) weapon.getType();
        if (weapon.getLinked() == null) return 0.0f;
        AmmoType at = (AmmoType) weapon.getLinked().getType();

        float fHits = determineClusterHits(wt);
        fHits = adjustForAMS(fHits, waa);
        return fHits * at.getDamagePerShot() * fChance;
    }

    private static float determineClusterHits(WeaponType wt) {
        if (wt.getAmmoType() == AmmoType.T_SRM_STREAK || wt.getAmmoType() == AmmoType.T_LRM_STREAK) {
            return wt.getRackSize();
        }
        if (wt.getRackSize() == 40 || wt.getRackSize() == 30) {
            return 2.0f * expectedHitsByRackSize[wt.getRackSize() / 2];
        }
        return expectedHitsByRackSize[wt.getRackSize()];
    }

    private static float adjustForAMS(float fHits, WeaponAttackAction waa) {
        List<WeaponMounted> vCounters = waa.getCounterEquipment();
        if (vCounters == null) return fHits;
        for (WeaponMounted vCounter : vCounters) {
            if (vCounter.getType().hasFlag(WeaponType.F_AMS)) {
                fHits = Math.max(0.0f, fHits - 3.5f * vCounter.getType().getDamage());
            }
        }
        return fHits;
    }

    /**
     * If the unit has stealth armor, turning it off is probably a good idea if
     * most of the enemy force is at 'short' range or if in danger of
     * overheating
     */

    protected void toggleStealth() {
        initialize();
        for (Entity entity : game.getEntitiesVector()) {
            if (shouldToggleStealth(entity)) {
                int newStealthMode = determineStealthMode(entity);
                applyStealthMode(entity, newStealthMode);
            }
        }
    }

    private boolean shouldToggleStealth(Entity entity) {
        return entity.getOwnerId() == localPlayerNumber && entity.hasStealth();
    }

    private int determineStealthMode(Entity entity) {
        if (!entity.tracksHeat()) return 1; // Always activate stealth if heat isn't an issue
        if (entity.heat > 13 + Compute.randomInt(7)) return 0; // Disable if overheating
        if (entity.getPosition() == null) return 1; // Enable if off-board

        return evaluateStealthBasedOnEnemies(entity);
    }

    private int evaluateStealthBasedOnEnemies(Entity entity) {
        int totalBV = 0, knownBV = 0, knownRange = 0, knownCount = 0;
        for (Entity enemy : game.getEntitiesVector()) {
            if (entity.isEnemyOf(enemy)) {
                totalBV += enemy.calculateBattleValue();
                if (enemy.isVisibleToEnemy()) {
                    knownCount++;
                    knownBV += enemy.calculateBattleValue();
                    knownRange += Compute.effectiveDistance(game, entity, enemy);
                }
            }
        }
        if (knownCount == 0 || knownBV < totalBV / 2) return 1; // Default to stealth on if enemies are hiding
        return (knownRange / knownCount) <= (5 + Compute.randomInt(5)) ? 0 : 1;
    }

    private void applyStealthMode(Entity entity, int newStealthMode) {
        for (Mounted<?> equipment : entity.getMisc()) {
            if (((MiscType) equipment.getType()).hasFlag(MiscType.F_STEALTH)) {
                equipment.setMode(newStealthMode);
                sendModeChange(entity.getId(), entity.getEquipmentNum(equipment), newStealthMode);
                break;
            }
        }
    }


    private @Nullable String getRandomBotMessage() {
        String message = null;

        try (FileInputStream fis = new FileInputStream("./mmconf/botmessages.txt"); // TODO : Remove inline file path
              InputStreamReader isr = new InputStreamReader(fis);
              BufferedReader br = new BufferedReader(isr)) {
            while (br.ready()) {
                message = br.readLine();
                if (Compute.randomInt(10) == 1) {
                    break;
                }
            }
        } catch (FileNotFoundException ignored) {
            // Don't do anything, just return a null and allow the bot to remain silent
            return null;
        } catch (Exception ex) {
            logger.error(ex, "Error while reading ./mmconf/botmessages.txt");
            return null;
        }

        return message;
    }

    /**
     * Pops up a dialog box showing an alert
     */
    public void doAlertDialog(String title, String message) {
        JTextPane textArea = new JTextPane();
        Report.setupStylesheet(textArea);

        textArea.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(textArea,
              ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
              ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        textArea.setText("<pre>" + message + "</pre>");
        JOptionPane.showMessageDialog(frame, scrollPane, title, JOptionPane.ERROR_MESSAGE);
    }

    @Override
    protected void correctName(Packet inP) {
        // If we have a clientGUI, it keeps track of a Name -> Client map, and
        // we need to update that map with this name change.
        if (getClientGUI() != null) {
            Map<String, AbstractClient> bots = getClientGUI().getLocalBots();
            String oldName = getName();
            String newName = (String) (inP.getObject(0));
            if (!this.equals(bots.get(oldName))) {
                logger.error("Name correction arrived at incorrect BotClient!");
                return;
            }
            bots.remove(oldName);
            bots.put(newName, this);
        }
        setName((String) (inP.getObject(0)));
    }

    private ClientGUI getClientGUI() {
        return clientGUI;
    }

    public void setClientGUI(ClientGUI clientGUI) {
        this.clientGUI = clientGUI;
    }

    public void endOfTurnProcessing() {
        // Do nothing;
    }

    private record MinefieldNumbers(int number, int type) {}

    /**
     * Deploy minefields for the bot
     */
    protected void deployMinefields() {
        MinefieldNumbers[] minefieldNumbers = getMinefieldNumbers();
        int totalMines = Arrays.stream(minefieldNumbers).mapToInt(MinefieldNumbers::number).sum();
        Deque<Coords> coordsSet = getMinefieldDeploymentPlanner().getRandomMinefieldPositions(totalMines);
        Vector<Minefield> deployedMinefields = new Vector<>();
        for (MinefieldNumbers minefieldNumber : minefieldNumbers) {
            deployMinefields(minefieldNumber, coordsSet, deployedMinefields);
        }
        performMinefieldDeployment(deployedMinefields);
    }

    /**
     * Deploy the specified number of minefields
     * @param deployedMinefields the vector to add the deployed minefields to
     */
    private void performMinefieldDeployment(Vector<Minefield> deployedMinefields) {
        sendDeployMinefields(deployedMinefields);
        resetMinefieldCounters();
    }

    /**
     * Reset the minefield counters for the bot and push the updated player info to the server
     */
    private void resetMinefieldCounters() {
        getLocalPlayer().setNbrMFActive(0);
        getLocalPlayer().setNbrMFCommand(0);
        getLocalPlayer().setNbrMFConventional(0);
        getLocalPlayer().setNbrMFInferno(0);
        getLocalPlayer().setNbrMFVibra(0);
        sendPlayerInfo();
    }

    /**
     * Deploy the specified number of minefields of the specified type
     * @param minefieldNumber the number of minefields to deploy and the type of minefield to deploy
     * @param coordsSet the set of coordinates to deploy the minefields to
     * @param deployedMinefields the vector to add the deployed minefields to
     */
    private void deployMinefields(MinefieldNumbers minefieldNumber, Deque<Coords> coordsSet, Vector<Minefield> deployedMinefields) {
        int minesToDeploy = minefieldNumber.number();
        while(!coordsSet.isEmpty() && minesToDeploy > 0) {
            Coords coords = coordsSet.poll();
            int density = Compute.randomIntInclusive(30) + 5;
            Minefield minefield = Minefield.createMinefield(
                  coords, getLocalPlayer().getId(), minefieldNumber.type(), density);
            deployedMinefields.add(minefield);
            minesToDeploy--;
        }
    }

    /**
     * Get the number of minefields of each type that the bot should deploy
     * @return an array of MinefieldNumbers, each representing the number of a specific type of minefield to deploy
     */
    private MinefieldNumbers[] getMinefieldNumbers() {
        return new MinefieldNumbers[]{
              new MinefieldNumbers(getLocalPlayer().getNbrMFActive(), Minefield.TYPE_ACTIVE),
              new MinefieldNumbers(getLocalPlayer().getNbrMFInferno(), Minefield.TYPE_INFERNO),
              new MinefieldNumbers(getLocalPlayer().getNbrMFConventional(), Minefield.TYPE_CONVENTIONAL),
              new MinefieldNumbers(getLocalPlayer().getNbrMFVibra(), Minefield.TYPE_VIBRABOMB),
              // the following are added for completeness, but are not used by the bot
              new MinefieldNumbers(0, Minefield.TYPE_COMMAND_DETONATED), // no command detonated mines
              new MinefieldNumbers(0, Minefield.TYPE_EMP), // no field for EMP mines exists
        };
    }

    /**
     * Get the minefield deployment planner to use for this bot
     * @return the minefield deployment planner
     */
    protected MinefieldDeploymentPlanner getMinefieldDeploymentPlanner() {
        return new RandomMinefieldDeploymentPlanner(getBoard());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void receiveBuildingCollapse(Packet packet) {
        game.getBoard().collapseBuilding((Vector<Coords>) packet.getObject(0));
    }

    /**
     * The bot client doesn't really need a text report
     * Let's save ourselves a little processing time and not deal with any of it
     */
    @Override
    public String receiveReport(List<Report> reports) {
        return "";
    }

    /**
     * The bot client has no need of image tag caching
     * Let's save ourselves some CPU and memory and not deal with it
     */
    @Override
    protected void cacheImgTag(Entity entity) {

    }

    public BoardClusterTracker getClusterTracker() {
        return boardClusterTracker;
    }

    private static class RankedCoords implements Comparable<RankedCoords> {
        private Coords coords;
        private double fitness;

        RankedCoords(Coords coords, double fitness) {
            if (coords == null) {
                throw new IllegalArgumentException("Coords cannot be null.");
            }
            this.coords = coords;
            this.fitness = fitness;
        }

        public Coords getCoords() {
            return coords;
        }

        @SuppressWarnings("unused")
        public void setCoords(Coords coords) {
            if (coords == null) {
                throw new IllegalArgumentException("Coords cannot be null.");
            }
            this.coords = coords;
        }

        public double getFitness() {
            return fitness;
        }

        public void setFitness(double fitness) {
            this.fitness = fitness;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof RankedCoords)) {
                return false;
            }

            RankedCoords coords1 = (RankedCoords) o;

            if (Double.compare(coords1.fitness, fitness) != 0) {
                return false;
            }
            // noinspection RedundantIfStatement
            if (!coords.equals(coords1.coords)) {
                return false;
            }

            return true;
        }

        @Override
        public int hashCode() {
            long temp = Double.doubleToLongBits(fitness);
            return 31 * coords.hashCode() + (int) (temp ^ (temp >>> 32));
        }

        @Override
        public String toString() {
            return String.format("RankedCoords { coords=%s, fitness=%f }", coords, fitness);
        }

        int getX() {
            return coords.getX();
        }

        int getY() {
            return coords.getY();
        }

        @Override
        public int compareTo(RankedCoords o) {
            return -Double.compare(getFitness(), o.getFitness());
        }
    }
}

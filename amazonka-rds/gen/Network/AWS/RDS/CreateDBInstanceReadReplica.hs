{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstanceReadReplica
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a DB instance for a DB instance running MySQL, MariaDB, or PostgreSQL that acts as a Read Replica of a source DB instance.
--
--
-- All Read Replica DB instances are created as Single-AZ deployments with backups disabled. All other DB instance attributes (including DB security groups and DB parameter groups) are inherited from the source DB instance, except as specified below.
--
-- /Important:/ The source DB instance must have backup retention enabled.
--
module Network.AWS.RDS.CreateDBInstanceReadReplica
    (
    -- * Creating a Request
      createDBInstanceReadReplica
    , CreateDBInstanceReadReplica
    -- * Request Lenses
    , cdirrPubliclyAccessible
    , cdirrAutoMinorVersionUpgrade
    , cdirrDBSubnetGroupName
    , cdirrMonitoringRoleARN
    , cdirrIOPS
    , cdirrMonitoringInterval
    , cdirrDBInstanceClass
    , cdirrAvailabilityZone
    , cdirrOptionGroupName
    , cdirrCopyTagsToSnapshot
    , cdirrTags
    , cdirrPort
    , cdirrStorageType
    , cdirrDBInstanceIdentifier
    , cdirrSourceDBInstanceIdentifier

    -- * Destructuring the Response
    , createDBInstanceReadReplicaResponse
    , CreateDBInstanceReadReplicaResponse
    -- * Response Lenses
    , cdirrrsDBInstance
    , cdirrrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
    { _cdirrPubliclyAccessible         :: !(Maybe Bool)
    , _cdirrAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _cdirrDBSubnetGroupName          :: !(Maybe Text)
    , _cdirrMonitoringRoleARN          :: !(Maybe Text)
    , _cdirrIOPS                       :: !(Maybe Int)
    , _cdirrMonitoringInterval         :: !(Maybe Int)
    , _cdirrDBInstanceClass            :: !(Maybe Text)
    , _cdirrAvailabilityZone           :: !(Maybe Text)
    , _cdirrOptionGroupName            :: !(Maybe Text)
    , _cdirrCopyTagsToSnapshot         :: !(Maybe Bool)
    , _cdirrTags                       :: !(Maybe [Tag])
    , _cdirrPort                       :: !(Maybe Int)
    , _cdirrStorageType                :: !(Maybe Text)
    , _cdirrDBInstanceIdentifier       :: !Text
    , _cdirrSourceDBInstanceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBInstanceReadReplica' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirrPubliclyAccessible' - Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be private.
--
-- * 'cdirrAutoMinorVersionUpgrade' - Indicates that minor engine upgrades will be applied automatically to the Read Replica during the maintenance window. Default: Inherits from the source DB instance
--
-- * 'cdirrDBSubnetGroupName' - Specifies a DB subnet group for the DB instance. The new DB instance will be created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance is not created in a VPC. Constraints:     * Can only be specified if the source DB instance identifier specifies a DB instance in another region.     * The specified DB subnet group must be in the same region in which the operation is running.     * All Read Replicas in one region that are created from the same source DB instance must either:>     * Specify DB subnet groups from the same VPC. All these Read Replicas will be created in the same VPC.     * Not specify a DB subnet group. All these Read Replicas will be created outside of any VPC. Constraints: Must contain no more than 255 alphanumeric characters, periods, underscores, spaces, or hyphens. Must not be default. Example: @mySubnetgroup@
--
-- * 'cdirrMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- * 'cdirrIOPS' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- * 'cdirrMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the Read Replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- * 'cdirrDBInstanceClass' - The compute and memory capacity of the Read Replica. Valid Values: @db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.m4.large | db.m4.xlarge | db.m4.2xlarge | db.m4.4xlarge | db.m4.10xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium | db.t2.large@  Default: Inherits from the source DB instance.
--
-- * 'cdirrAvailabilityZone' - The Amazon EC2 Availability Zone that the Read Replica will be created in. Default: A random, system-chosen Availability Zone in the endpoint's region. Example: @us-east-1d@
--
-- * 'cdirrOptionGroupName' - The option group the DB instance will be associated with. If omitted, the default option group for the engine specified will be used.
--
-- * 'cdirrCopyTagsToSnapshot' - True to copy all tags from the Read Replica to snapshots of the Read Replica; otherwise false. The default is false.
--
-- * 'cdirrTags' - Undocumented member.
--
-- * 'cdirrPort' - The port number that the DB instance uses for connections. Default: Inherits from the source DB instance Valid Values: @1150-65535@
--
-- * 'cdirrStorageType' - Specifies the storage type to be associated with the Read Replica. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @standard@
--
-- * 'cdirrDBInstanceIdentifier' - The DB instance identifier of the Read Replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
--
-- * 'cdirrSourceDBInstanceIdentifier' - The identifier of the DB instance that will act as the source for the Read Replica. Each DB instance can have up to five Read Replicas. Constraints:     * Must be the identifier of an existing MySQL, MariaDB, or PostgreSQL DB instance.     * Can specify a DB instance that is a MySQL Read Replica only if the source is running MySQL 5.6.     * Can specify a DB instance that is a PostgreSQL Read Replica only if the source is running PostgreSQL 9.3.5.     * The specified DB instance must have automatic backups enabled, its backup retention period must be greater than 0.     * If the source DB instance is in the same region as the Read Replica, specify a valid DB instance identifier.     * If the source DB instance is in a different region than the Read Replica, specify a valid DB instance ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing a Amazon RDS Amazon Resource Name (ARN)> .
createDBInstanceReadReplica
    :: Text -- ^ 'cdirrDBInstanceIdentifier'
    -> Text -- ^ 'cdirrSourceDBInstanceIdentifier'
    -> CreateDBInstanceReadReplica
createDBInstanceReadReplica pDBInstanceIdentifier_ pSourceDBInstanceIdentifier_ =
    CreateDBInstanceReadReplica'
    { _cdirrPubliclyAccessible = Nothing
    , _cdirrAutoMinorVersionUpgrade = Nothing
    , _cdirrDBSubnetGroupName = Nothing
    , _cdirrMonitoringRoleARN = Nothing
    , _cdirrIOPS = Nothing
    , _cdirrMonitoringInterval = Nothing
    , _cdirrDBInstanceClass = Nothing
    , _cdirrAvailabilityZone = Nothing
    , _cdirrOptionGroupName = Nothing
    , _cdirrCopyTagsToSnapshot = Nothing
    , _cdirrTags = Nothing
    , _cdirrPort = Nothing
    , _cdirrStorageType = Nothing
    , _cdirrDBInstanceIdentifier = pDBInstanceIdentifier_
    , _cdirrSourceDBInstanceIdentifier = pSourceDBInstanceIdentifier_
    }

-- | Specifies the accessibility options for the DB instance. A value of true specifies an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. A value of false specifies an internal instance with a DNS name that resolves to a private IP address. Default: The default behavior varies depending on whether a VPC has been requested or not. The following list shows the default behavior in each case.     * __Default VPC:__ true     * __VPC:__ false If no DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be publicly accessible. If a specific DB subnet group has been specified as part of the request and the PubliclyAccessible value has not been set, the DB instance will be private.
cdirrPubliclyAccessible :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrPubliclyAccessible = lens _cdirrPubliclyAccessible (\ s a -> s{_cdirrPubliclyAccessible = a});

-- | Indicates that minor engine upgrades will be applied automatically to the Read Replica during the maintenance window. Default: Inherits from the source DB instance
cdirrAutoMinorVersionUpgrade :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrAutoMinorVersionUpgrade = lens _cdirrAutoMinorVersionUpgrade (\ s a -> s{_cdirrAutoMinorVersionUpgrade = a});

-- | Specifies a DB subnet group for the DB instance. The new DB instance will be created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance is not created in a VPC. Constraints:     * Can only be specified if the source DB instance identifier specifies a DB instance in another region.     * The specified DB subnet group must be in the same region in which the operation is running.     * All Read Replicas in one region that are created from the same source DB instance must either:>     * Specify DB subnet groups from the same VPC. All these Read Replicas will be created in the same VPC.     * Not specify a DB subnet group. All these Read Replicas will be created outside of any VPC. Constraints: Must contain no more than 255 alphanumeric characters, periods, underscores, spaces, or hyphens. Must not be default. Example: @mySubnetgroup@
cdirrDBSubnetGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDBSubnetGroupName = lens _cdirrDBSubnetGroupName (\ s a -> s{_cdirrDBSubnetGroupName = a});

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> . If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
cdirrMonitoringRoleARN :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrMonitoringRoleARN = lens _cdirrMonitoringRoleARN (\ s a -> s{_cdirrMonitoringRoleARN = a});

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
cdirrIOPS :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrIOPS = lens _cdirrIOPS (\ s a -> s{_cdirrIOPS = a});

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the Read Replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@
cdirrMonitoringInterval :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrMonitoringInterval = lens _cdirrMonitoringInterval (\ s a -> s{_cdirrMonitoringInterval = a});

-- | The compute and memory capacity of the Read Replica. Valid Values: @db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge | db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge | db.m3.medium | db.m3.large | db.m3.xlarge | db.m3.2xlarge | db.m4.large | db.m4.xlarge | db.m4.2xlarge | db.m4.4xlarge | db.m4.10xlarge | db.r3.large | db.r3.xlarge | db.r3.2xlarge | db.r3.4xlarge | db.r3.8xlarge | db.t2.micro | db.t2.small | db.t2.medium | db.t2.large@  Default: Inherits from the source DB instance.
cdirrDBInstanceClass :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrDBInstanceClass = lens _cdirrDBInstanceClass (\ s a -> s{_cdirrDBInstanceClass = a});

-- | The Amazon EC2 Availability Zone that the Read Replica will be created in. Default: A random, system-chosen Availability Zone in the endpoint's region. Example: @us-east-1d@
cdirrAvailabilityZone :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrAvailabilityZone = lens _cdirrAvailabilityZone (\ s a -> s{_cdirrAvailabilityZone = a});

-- | The option group the DB instance will be associated with. If omitted, the default option group for the engine specified will be used.
cdirrOptionGroupName :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrOptionGroupName = lens _cdirrOptionGroupName (\ s a -> s{_cdirrOptionGroupName = a});

-- | True to copy all tags from the Read Replica to snapshots of the Read Replica; otherwise false. The default is false.
cdirrCopyTagsToSnapshot :: Lens' CreateDBInstanceReadReplica (Maybe Bool)
cdirrCopyTagsToSnapshot = lens _cdirrCopyTagsToSnapshot (\ s a -> s{_cdirrCopyTagsToSnapshot = a});

-- | Undocumented member.
cdirrTags :: Lens' CreateDBInstanceReadReplica [Tag]
cdirrTags = lens _cdirrTags (\ s a -> s{_cdirrTags = a}) . _Default . _Coerce;

-- | The port number that the DB instance uses for connections. Default: Inherits from the source DB instance Valid Values: @1150-65535@
cdirrPort :: Lens' CreateDBInstanceReadReplica (Maybe Int)
cdirrPort = lens _cdirrPort (\ s a -> s{_cdirrPort = a});

-- | Specifies the storage type to be associated with the Read Replica. Valid values: @standard | gp2 | io1@  If you specify @io1@ , you must also include a value for the @Iops@ parameter.  Default: @io1@ if the @Iops@ parameter is specified; otherwise @standard@
cdirrStorageType :: Lens' CreateDBInstanceReadReplica (Maybe Text)
cdirrStorageType = lens _cdirrStorageType (\ s a -> s{_cdirrStorageType = a});

-- | The DB instance identifier of the Read Replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
cdirrDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrDBInstanceIdentifier = lens _cdirrDBInstanceIdentifier (\ s a -> s{_cdirrDBInstanceIdentifier = a});

-- | The identifier of the DB instance that will act as the source for the Read Replica. Each DB instance can have up to five Read Replicas. Constraints:     * Must be the identifier of an existing MySQL, MariaDB, or PostgreSQL DB instance.     * Can specify a DB instance that is a MySQL Read Replica only if the source is running MySQL 5.6.     * Can specify a DB instance that is a PostgreSQL Read Replica only if the source is running PostgreSQL 9.3.5.     * The specified DB instance must have automatic backups enabled, its backup retention period must be greater than 0.     * If the source DB instance is in the same region as the Read Replica, specify a valid DB instance identifier.     * If the source DB instance is in a different region than the Read Replica, specify a valid DB instance ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing a Amazon RDS Amazon Resource Name (ARN)> .
cdirrSourceDBInstanceIdentifier :: Lens' CreateDBInstanceReadReplica Text
cdirrSourceDBInstanceIdentifier = lens _cdirrSourceDBInstanceIdentifier (\ s a -> s{_cdirrSourceDBInstanceIdentifier = a});

instance AWSRequest CreateDBInstanceReadReplica where
        type Rs CreateDBInstanceReadReplica =
             CreateDBInstanceReadReplicaResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "CreateDBInstanceReadReplicaResult"
              (\ s h x ->
                 CreateDBInstanceReadReplicaResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable CreateDBInstanceReadReplica

instance NFData CreateDBInstanceReadReplica

instance ToHeaders CreateDBInstanceReadReplica where
        toHeaders = const mempty

instance ToPath CreateDBInstanceReadReplica where
        toPath = const "/"

instance ToQuery CreateDBInstanceReadReplica where
        toQuery CreateDBInstanceReadReplica'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBInstanceReadReplica" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "PubliclyAccessible" =: _cdirrPubliclyAccessible,
               "AutoMinorVersionUpgrade" =:
                 _cdirrAutoMinorVersionUpgrade,
               "DBSubnetGroupName" =: _cdirrDBSubnetGroupName,
               "MonitoringRoleArn" =: _cdirrMonitoringRoleARN,
               "Iops" =: _cdirrIOPS,
               "MonitoringInterval" =: _cdirrMonitoringInterval,
               "DBInstanceClass" =: _cdirrDBInstanceClass,
               "AvailabilityZone" =: _cdirrAvailabilityZone,
               "OptionGroupName" =: _cdirrOptionGroupName,
               "CopyTagsToSnapshot" =: _cdirrCopyTagsToSnapshot,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdirrTags),
               "Port" =: _cdirrPort,
               "StorageType" =: _cdirrStorageType,
               "DBInstanceIdentifier" =: _cdirrDBInstanceIdentifier,
               "SourceDBInstanceIdentifier" =:
                 _cdirrSourceDBInstanceIdentifier]

-- | /See:/ 'createDBInstanceReadReplicaResponse' smart constructor.
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
    { _cdirrrsDBInstance     :: !(Maybe DBInstance)
    , _cdirrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBInstanceReadReplicaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdirrrsDBInstance' - Undocumented member.
--
-- * 'cdirrrsResponseStatus' - -- | The response status code.
createDBInstanceReadReplicaResponse
    :: Int -- ^ 'cdirrrsResponseStatus'
    -> CreateDBInstanceReadReplicaResponse
createDBInstanceReadReplicaResponse pResponseStatus_ =
    CreateDBInstanceReadReplicaResponse'
    { _cdirrrsDBInstance = Nothing
    , _cdirrrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cdirrrsDBInstance :: Lens' CreateDBInstanceReadReplicaResponse (Maybe DBInstance)
cdirrrsDBInstance = lens _cdirrrsDBInstance (\ s a -> s{_cdirrrsDBInstance = a});

-- | -- | The response status code.
cdirrrsResponseStatus :: Lens' CreateDBInstanceReadReplicaResponse Int
cdirrrsResponseStatus = lens _cdirrrsResponseStatus (\ s a -> s{_cdirrrsResponseStatus = a});

instance NFData CreateDBInstanceReadReplicaResponse

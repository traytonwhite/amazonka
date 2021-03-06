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
-- Module      : Network.AWS.IAM.DeleteGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM group. The group must not contain any users or have any attached policies.
--
--
module Network.AWS.IAM.DeleteGroup
    (
    -- * Creating a Request
      deleteGroup
    , DeleteGroup
    -- * Request Lenses
    , dgGroupName

    -- * Destructuring the Response
    , deleteGroupResponse
    , DeleteGroupResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteGroup' smart constructor.
newtype DeleteGroup = DeleteGroup'
    { _dgGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgGroupName' - The name of the IAM group to delete. The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
deleteGroup
    :: Text -- ^ 'dgGroupName'
    -> DeleteGroup
deleteGroup pGroupName_ =
    DeleteGroup'
    { _dgGroupName = pGroupName_
    }

-- | The name of the IAM group to delete. The <http://wikipedia.org/wiki/regex regex pattern> for this parameter is a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
dgGroupName :: Lens' DeleteGroup Text
dgGroupName = lens _dgGroupName (\ s a -> s{_dgGroupName = a});

instance AWSRequest DeleteGroup where
        type Rs DeleteGroup = DeleteGroupResponse
        request = postQuery iam
        response = receiveNull DeleteGroupResponse'

instance Hashable DeleteGroup

instance NFData DeleteGroup

instance ToHeaders DeleteGroup where
        toHeaders = const mempty

instance ToPath DeleteGroup where
        toPath = const "/"

instance ToQuery DeleteGroup where
        toQuery DeleteGroup'{..}
          = mconcat
              ["Action" =: ("DeleteGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _dgGroupName]

-- | /See:/ 'deleteGroupResponse' smart constructor.
data DeleteGroupResponse =
    DeleteGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
--
deleteGroupResponse
    :: DeleteGroupResponse
deleteGroupResponse = DeleteGroupResponse'

instance NFData DeleteGroupResponse

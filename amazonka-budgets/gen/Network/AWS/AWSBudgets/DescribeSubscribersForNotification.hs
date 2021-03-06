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
-- Module      : Network.AWS.AWSBudgets.DescribeSubscribersForNotification
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get subscribers of a notification
module Network.AWS.AWSBudgets.DescribeSubscribersForNotification
    (
    -- * Creating a Request
      describeSubscribersForNotification
    , DescribeSubscribersForNotification
    -- * Request Lenses
    , dsfnNextToken
    , dsfnMaxResults
    , dsfnAccountId
    , dsfnBudgetName
    , dsfnNotification

    -- * Destructuring the Response
    , describeSubscribersForNotificationResponse
    , DescribeSubscribersForNotificationResponse
    -- * Response Lenses
    , dsfnrsNextToken
    , dsfnrsSubscribers
    , dsfnrsResponseStatus
    ) where

import           Network.AWS.AWSBudgets.Types
import           Network.AWS.AWSBudgets.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Request of DescribeSubscribersForNotification
--
-- /See:/ 'describeSubscribersForNotification' smart constructor.
data DescribeSubscribersForNotification = DescribeSubscribersForNotification'
    { _dsfnNextToken    :: !(Maybe Text)
    , _dsfnMaxResults   :: !(Maybe Nat)
    , _dsfnAccountId    :: !Text
    , _dsfnBudgetName   :: !Text
    , _dsfnNotification :: !Notification
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSubscribersForNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfnNextToken' - Undocumented member.
--
-- * 'dsfnMaxResults' - Undocumented member.
--
-- * 'dsfnAccountId' - Undocumented member.
--
-- * 'dsfnBudgetName' - Undocumented member.
--
-- * 'dsfnNotification' - Undocumented member.
describeSubscribersForNotification
    :: Text -- ^ 'dsfnAccountId'
    -> Text -- ^ 'dsfnBudgetName'
    -> Notification -- ^ 'dsfnNotification'
    -> DescribeSubscribersForNotification
describeSubscribersForNotification pAccountId_ pBudgetName_ pNotification_ =
    DescribeSubscribersForNotification'
    { _dsfnNextToken = Nothing
    , _dsfnMaxResults = Nothing
    , _dsfnAccountId = pAccountId_
    , _dsfnBudgetName = pBudgetName_
    , _dsfnNotification = pNotification_
    }

-- | Undocumented member.
dsfnNextToken :: Lens' DescribeSubscribersForNotification (Maybe Text)
dsfnNextToken = lens _dsfnNextToken (\ s a -> s{_dsfnNextToken = a});

-- | Undocumented member.
dsfnMaxResults :: Lens' DescribeSubscribersForNotification (Maybe Natural)
dsfnMaxResults = lens _dsfnMaxResults (\ s a -> s{_dsfnMaxResults = a}) . mapping _Nat;

-- | Undocumented member.
dsfnAccountId :: Lens' DescribeSubscribersForNotification Text
dsfnAccountId = lens _dsfnAccountId (\ s a -> s{_dsfnAccountId = a});

-- | Undocumented member.
dsfnBudgetName :: Lens' DescribeSubscribersForNotification Text
dsfnBudgetName = lens _dsfnBudgetName (\ s a -> s{_dsfnBudgetName = a});

-- | Undocumented member.
dsfnNotification :: Lens' DescribeSubscribersForNotification Notification
dsfnNotification = lens _dsfnNotification (\ s a -> s{_dsfnNotification = a});

instance AWSRequest
         DescribeSubscribersForNotification where
        type Rs DescribeSubscribersForNotification =
             DescribeSubscribersForNotificationResponse
        request = postJSON awsBudgets
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSubscribersForNotificationResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Subscribers") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeSubscribersForNotification

instance NFData DescribeSubscribersForNotification

instance ToHeaders DescribeSubscribersForNotification
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.DescribeSubscribersForNotification"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSubscribersForNotification
         where
        toJSON DescribeSubscribersForNotification'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dsfnNextToken,
                  ("MaxResults" .=) <$> _dsfnMaxResults,
                  Just ("AccountId" .= _dsfnAccountId),
                  Just ("BudgetName" .= _dsfnBudgetName),
                  Just ("Notification" .= _dsfnNotification)])

instance ToPath DescribeSubscribersForNotification
         where
        toPath = const "/"

instance ToQuery DescribeSubscribersForNotification
         where
        toQuery = const mempty

-- | Response of DescribeSubscribersForNotification
--
-- /See:/ 'describeSubscribersForNotificationResponse' smart constructor.
data DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse'
    { _dsfnrsNextToken      :: !(Maybe Text)
    , _dsfnrsSubscribers    :: !(Maybe (List1 Subscriber))
    , _dsfnrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeSubscribersForNotificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsfnrsNextToken' - Undocumented member.
--
-- * 'dsfnrsSubscribers' - Undocumented member.
--
-- * 'dsfnrsResponseStatus' - -- | The response status code.
describeSubscribersForNotificationResponse
    :: Int -- ^ 'dsfnrsResponseStatus'
    -> DescribeSubscribersForNotificationResponse
describeSubscribersForNotificationResponse pResponseStatus_ =
    DescribeSubscribersForNotificationResponse'
    { _dsfnrsNextToken = Nothing
    , _dsfnrsSubscribers = Nothing
    , _dsfnrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dsfnrsNextToken :: Lens' DescribeSubscribersForNotificationResponse (Maybe Text)
dsfnrsNextToken = lens _dsfnrsNextToken (\ s a -> s{_dsfnrsNextToken = a});

-- | Undocumented member.
dsfnrsSubscribers :: Lens' DescribeSubscribersForNotificationResponse (Maybe (NonEmpty Subscriber))
dsfnrsSubscribers = lens _dsfnrsSubscribers (\ s a -> s{_dsfnrsSubscribers = a}) . mapping _List1;

-- | -- | The response status code.
dsfnrsResponseStatus :: Lens' DescribeSubscribersForNotificationResponse Int
dsfnrsResponseStatus = lens _dsfnrsResponseStatus (\ s a -> s{_dsfnrsResponseStatus = a});

instance NFData
         DescribeSubscribersForNotificationResponse
